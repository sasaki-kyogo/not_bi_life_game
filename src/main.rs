use image::{GenericImageView, ImageError, Pixel, Rgb};
use uuid::Uuid;

use core::fmt;
use std::fs::{create_dir, remove_dir_all, rename};
use std::io::{stdout, ErrorKind, Write};

use std::path::Path;
use std::process::{Command, Stdio};
use std::sync::mpsc;
use std::thread;
use std::time::Instant;

use clap::{Parser, ValueEnum};
use regex::{self, Regex};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// 動画のフレームレートを設定します。最大で60fpsまでです。
    #[arg(long, default_value_t = 24)]
    fps: u32,

    /// 動画のフレーム数を設定します。秒数はfpsに依存します。最大で10^8未満です。
    #[arg(long, default_value_t = 600)]
    frame: u32,

    /// ライフゲームの元となる入力画像を設定します。
    #[arg(long, short)]
    input: String,

    /// 出力するパスを設定します。
    #[arg(long, short)]
    output: String,

    /// 出力するファイルタイプを設定します。
    #[arg(long, default_value_t = OutputTypeChoices::Movie)]
    file_type: OutputTypeChoices,

    /// ファイル名／フォルダ名にuuidをつけるかを設定します。
    /// ファイルの上書きしたくないときに役立ちます。
    #[arg(long)]
    need_uuid: bool,

    /// ライフゲームのルールを設定します。
    /// `23/3`は2または3ならば生存し、3ならば誕生することを意味します。
    #[arg(long, default_value_t = String::from("23/3"))]
    rule: String,

    /// 一世代前のフレームに対して、現世代のフレームの影響力を設定します。
    /// 0だとオリジナルの画像を維持、1だと一世代前の影響を受けなくなります。
    #[arg(long)]
    wet: f64,

    /// 色を変更する際の参照元を設定します。
    #[arg(long,default_value_t=ReferenceImgChoices::Previous)]
    reference_img: ReferenceImgChoices,

    /// 周りに生存／死亡セルがないときに利用する値を設定します。
    #[arg(long,default_value_t=DefaultColorChoices::Binaly)]
    default_color: DefaultColorChoices,

    /// 周りのセルと同じセルがあったとき、そのセルを生存セルとみなす生存確度のしきい値を設定します。
    /// しきい値は0.0-1.0の間の値を取ります。
    #[arg(long, default_value_t = 0.5)]
    alive_threshold: f64,
}

#[derive(Debug, Clone, ValueEnum)]
enum OutputTypeChoices {
    Frame,
    Movie,
    Both,
}

impl fmt::Display for OutputTypeChoices {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let a = match *self {
            OutputTypeChoices::Frame => "frame",
            OutputTypeChoices::Movie => "movie",
            OutputTypeChoices::Both => "both",
        };

        write!(f, "{}", a)
    }
}

#[derive(Debug, Clone, ValueEnum)]
enum ReferenceImgChoices {
    /// オリジナルの画像を参照します。
    /// 一度、極端に暗い／明るい色になっても色相の情報が維持されます。
    Original,

    /// 一つ前の画像の色を参照します。
    /// 一度、極端に暗い／明るい色になると、色相の情報が損失しグレースケールになります。
    Previous,
}

impl fmt::Display for ReferenceImgChoices {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let a = match *self {
            ReferenceImgChoices::Original => "original",
            ReferenceImgChoices::Previous => "previous",
        };

        write!(f, "{}", a)
    }
}

#[derive(Debug, Clone, ValueEnum)]
enum DefaultColorChoices {
    /// 白黒をデフォルトにします。
    /// これを利用すると、`正しい`ライフゲームの拡張になります。
    Binaly,

    /// 自分自身に最も近い明るい／暗い色をデフォルトにします。
    /// ライフゲームの拡張ではないですが、カラー画像で利用した際に滑らかに明るさが変化します。
    Nearest,
}

impl fmt::Display for DefaultColorChoices {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let a = match *self {
            DefaultColorChoices::Binaly => "binaly",
            DefaultColorChoices::Nearest => "nearest",
        };

        write!(f, "{}", a)
    }
}

fn get_around(img: &image::DynamicImage, x: u32, y: u32) -> Vec<Rgb<u8>> {
    let (w, h) = img.dimensions();

    let mut around = Vec::new();

    for xh in -1..=1 {
        for yh in -1..=1 {
            if xh == 0 && yh == 0 {
                continue;
            }

            let xi = ((x as i32 + xh + 2 * w as i32) as u32) % w;
            let yi = ((y as i32 + yh + 2 * h as i32) as u32) % h;

            let c = img.get_pixel(xi, yi).to_rgb();
            around.push(c);
        }
    }

    around
}

fn change_luma(color: &Rgb<u8>, new_luma: u8) -> Rgb<u8> {
    let luma = color.to_luma()[0];
    let ratio = new_luma as f64 - luma as f64;
    let ratio = if ratio <= 0.0 {
        ratio / luma as f64
    } else {
        ratio / (255 - luma) as f64
    };

    *Rgb::from_slice(
        color
            .channels()
            .iter()
            .map(|c| {
                let dc = if ratio > 0.0 {
                    (255 - *c) as f64 * ratio
                } else {
                    *c as f64 * ratio
                };

                (*c as f64 + dc) as u8
            })
            .collect::<Vec<_>>()
            .as_slice(),
    )
}

/// (生存,誕生)
fn compile_rule(rule_txt: &str) -> (Vec<u32>, Vec<u32>) {
    let rule_regex = Regex::new(r"^(\d*)/(\d*)$").expect("正規表現の生成に失敗しました。");
    let cap = rule_regex
        .captures(rule_txt)
        .expect("不正なルールです。`xx/xx`のように指定してください。");

    let cap_to_nums = |idx| {
        cap.get(idx)
            .expect("ルールの解釈に失敗しました。")
            .as_str()
            .chars()
            .map(|c: char| -> u32 {
                let num = c as u32 - 48;

                if !(0..=8).contains(&num) {
                    panic!(
                        "不正なルールです。{}は不正な値です。0-8の間の数字のみ有効です。",
                        c
                    );
                }

                num
            })
            .collect::<Vec<_>>()
    };

    let alive = cap_to_nums(1);
    let born = cap_to_nums(2);

    (alive, born)
}

#[derive(Clone, Debug)]
struct LifeGameConfig {
    alive_nums: Vec<u32>,
    born_nums: Vec<u32>,
    default_color: DefaultColorChoices,
    alive_threshold: f64,
    wet: f64,
}

fn calc_life_game_luma(
    around: Vec<Rgb<u8>>,
    center: Rgb<u8>,
    ref_color: &Rgb<u8>,
    config: &LifeGameConfig,
) -> Rgb<u8> {
    let center_luma: u8 = center.to_luma()[0];

    let mut alive = 0;
    let mut _dead = 0;
    let mut equal = 0;

    let mut alive_min_luma = None;
    let mut dead_max_luma = None;

    for a in around {
        let luma = a.to_luma()[0];

        // 大きい値が生きている、小さい値が死んでいるとしたほうがわかりやすいので。
        match luma.cmp(&center_luma) {
            std::cmp::Ordering::Greater => {
                alive += 1;

                if let Some(min) = alive_min_luma {
                    if min > luma {
                        alive_min_luma = Some(luma);
                    }
                } else {
                    alive_min_luma = Some(luma);
                }
            }
            std::cmp::Ordering::Less => {
                _dead += 1;

                if let Some(max) = dead_max_luma {
                    if max < luma {
                        dead_max_luma = Some(luma);
                    }
                } else {
                    dead_max_luma = Some(luma);
                }
            }
            std::cmp::Ordering::Equal => {
                equal += 1;
            }
        }
    }

    // max,minが存在しないときは、少しでも自身のlumaが大きい/小さいなら生きる/死ぬこととする。
    // max=0,min=255でも動くけど、突然白黒に変化するのは微妙なので。
    let alive_min_luma = if let Some(min) = alive_min_luma {
        min
    } else {
        match config.default_color {
            DefaultColorChoices::Binaly => 255,
            DefaultColorChoices::Nearest => (center_luma as i32 + 1).clamp(0, 255) as u8,
        }
    };

    let dead_max_luma = if let Some(max) = dead_max_luma {
        max
    } else {
        match config.default_color {
            DefaultColorChoices::Binaly => 0,
            DefaultColorChoices::Nearest => (center_luma as i32 - 1).clamp(0, 255) as u8,
        }
    };

    // 生存確度（center_lumaがalive寄りかdead寄りかを計算する）
    let alive_confidently =
        (center_luma - dead_max_luma) as f64 / (alive_min_luma - dead_max_luma) as f64;

    if alive_confidently < config.alive_threshold {
        _dead += equal
    } else {
        alive += equal;
    }

    if config.born_nums.iter().any(|n| alive == *n) {
        change_luma(
            ref_color,
            (center_luma as f64 - (center_luma as f64 - alive_min_luma as f64) * config.wet) as u8,
        )
    } else if config.alive_nums.iter().any(|n| alive == *n) {
        center.to_rgb()
    } else {
        change_luma(
            ref_color,
            (center_luma as f64 - (center_luma as f64 - dead_max_luma as f64) * config.wet) as u8,
        )
    }
}

fn calc_life_game_rgb(around: Vec<Rgb<u8>>, center: Rgb<u8>, config: &LifeGameConfig) -> Rgb<u8> {
    // let around = get_around(img, x, y);
    // let center = img.get_pixel(x, y).to_rgb();

    let mut alive = [0; 3];
    let mut dead = [0; 3];
    let mut equal = [0; 3];

    let mut alive_mins = [None, None, None];
    let mut dead_maxs = [None, None, None];

    for a in &around {
        for (idx, channel) in a.channels().iter().enumerate() {
            // 大きい値が生きている、小さい値が死んでいるとしたほうがわかりやすいので。

            match channel.cmp(&center[idx]) {
                std::cmp::Ordering::Greater => {
                    alive[idx] += 1;

                    if let Some(min) = alive_mins[idx] {
                        if min > channel {
                            alive_mins[idx] = Some(channel);
                        }
                    } else {
                        alive_mins[idx] = Some(channel);
                    }
                }
                std::cmp::Ordering::Less => {
                    dead[idx] += 1;

                    if let Some(max) = dead_maxs[idx] {
                        if max < channel {
                            dead_maxs[idx] = Some(channel);
                        }
                    } else {
                        dead_maxs[idx] = Some(channel);
                    }
                }
                std::cmp::Ordering::Equal => {
                    equal[idx] += 1;
                }
            }
        }
    }

    let alive_mins = alive_mins
        .iter()
        .enumerate()
        .map(|(idx, a)| {
            if let Some(min) = *a {
                *min
            } else {
                match config.default_color {
                    DefaultColorChoices::Binaly => 255,
                    DefaultColorChoices::Nearest => (center[idx] as i32 + 1).clamp(0, 255) as u8,
                }
            }
        })
        .collect::<Vec<_>>();

    let dead_maxs = dead_maxs
        .iter()
        .enumerate()
        .map(|(idx, d)| {
            if let Some(max) = *d {
                *max
            } else {
                match config.default_color {
                    DefaultColorChoices::Binaly => 0,
                    DefaultColorChoices::Nearest => (center[idx] as i32 - 1).clamp(0, 255) as u8,
                }
            }
        })
        .collect::<Vec<_>>();

    // 生存確度（center_lumaがalive寄りかdead寄りかを計算する）
    let alive_confidentlies = center
        .channels()
        .iter()
        .enumerate()
        .map(|(idx, c)| (c - dead_maxs[idx]) as f64 / (alive_mins[idx] - dead_maxs[idx]) as f64)
        .collect::<Vec<_>>();

    for (idx, &alive_confidently) in alive_confidentlies.iter().enumerate() {
        if alive_confidently < config.alive_threshold {
            dead[idx] += equal[idx];
        } else {
            alive[idx] += equal[idx];
        }
    }

    let res = center
        .channels()
        .iter()
        .enumerate()
        .map(|(idx, &c)| {
            if config.born_nums.iter().any(|n| alive[idx] == *n) {
                (c as f64 - (c as f64 - alive_mins[idx] as f64) * config.wet) as u8
            } else if config.alive_nums.iter().any(|n| alive[idx] == *n) {
                center.channels()[idx]
            } else {
                (c as f64 - (c as f64 - dead_maxs[idx] as f64) * config.wet) as u8
            }
        })
        .collect::<Vec<_>>();

    *Rgb::from_slice(res.as_slice())
}

fn encode_frames_to_mp4(name: &str, path: &str, fps: u32) {
    // ffmpegを実行するためにコマンドを組み立てる
    let command = |path, output| {
        format!(
            "ffmpeg -framerate {} -i {} -codec:v libx264 -preset superfast -vf scale=1080:-1 -y {}.mp4",
            fps, path, output
        )
    };

    // ffmpegを実行
    let mut ffmpeg = Command::new("/bin/sh")
        .args(["-c", &command(path, name)])
        .stdin(Stdio::piped())
        .spawn()
        .expect("ffmpegの実行に失敗しました。");

    // ffmpeg側の終了を待つ
    ffmpeg.wait().expect("ffmpegが終了しませんでした。");
}

fn optimize_img_brightness(path: String, max: f64, min: f64) {
    let mut img = image::open(path)
        .expect("画像を開けませんでした。")
        .to_rgb8();

    for (_x, _y, pixel) in img.enumerate_pixels_mut() {
        let a = pixel
            .channels()
            .iter()
            .map(|c| ((*c as f64 - min) / (max - min) * 255.0) as u8)
            .map(|c| {
                let c = c as f64 / 255.0;
                (255.0 * c.powf(1.0 / 2.2)) as u8
            })
            .collect::<Vec<_>>();
        let a = Rgb::from_slice(a.as_slice());

        *pixel = *a;
    }
}

fn main() {
    let args = Args::parse();
    let (alive_nums, born_nums) = compile_rule(&args.rule);
    let life_game_config = LifeGameConfig {
        alive_nums,
        born_nums,
        default_color: args.default_color,
        alive_threshold: args.alive_threshold,
        wet: args.wet,
    };

    if args.frame >= 10_u32.pow(8) {
        panic!("フレーム数は10^8未満である必要があります。");
    }

    if args.fps > 60 {
        panic!("フレームレートは60fps以下である必要があります。");
    }

    if !(0.0..=1.0).contains(&args.wet) {
        panic!("ライフゲームの影響割合(wet)は0.0-1.0の間の値である必要があります。");
    }

    if !(0.0..=1.0).contains(&args.alive_threshold) {
        panic!("生存確度のしきい値(alive_threshold)は0.0-1.0の間である必要があります。")
    }

    let original_img = image::open(args.input.clone()).expect("画像を開けませんでした。");
    let (width, height) = original_img.dimensions();
    let mut prev = original_img.clone();

    let get_temp_path = |idx: u32| -> String { format!("temp/{:>08}.png", idx) };

    let path = get_temp_path(0);
    match prev.to_rgb8().save(&path) {
        Ok(_) => (),
        Err(ImageError::IoError(e)) if e.kind() == ErrorKind::NotFound => {
            create_dir("temp").expect("tempフォルダの生成に失敗しました。");
            prev.to_rgb8()
                .save(&path)
                .expect("画像の保存に失敗しました。");
        }
        Err(e) => panic!("画像の保存に失敗しました。{}", e),
    };

    // let (mut mean_min, mut mean_max) = (0_u32, 0_u32);
    // let (mut max, mut min) = (0_u8, 255_u8);

    let start = Instant::now();
    for i in 1..args.frame {
        let progress = ((i + 1) as f64 / args.frame as f64) * 1000.0;
        let progress = progress.trunc() / 10.0;
        print!(
            "calculating life game... {:>08}/{:>08} [{:03.1}%]\r",
            i + 1,
            args.frame,
            progress
        );
        stdout()
            .flush()
            .expect("バッファのフラッシュに失敗しました。");

        let mut imgbuf = image::ImageBuffer::new(width, height);

        let mut life_game_infos = Vec::new();

        // あんまりないけど、スレッド数よりピクセル数が少ないときに無駄なベクタを作らないようにする。
        for _i in 0..32.min(width * height) {
            life_game_infos.push(Vec::new());
        }

        for x in 0..width {
            for y in 0..height {
                let idx = (x * height + y) as usize % life_game_infos.len();

                life_game_infos[idx].push(((x, y), get_around(&prev, x, y), prev.get_pixel(x, y)));
            }
        }

        let (tx, rx) = mpsc::channel();
        for infos in life_game_infos {
            let tx_clone = tx.clone();
            let config = life_game_config.clone();
            thread::spawn(move || {
                let mut pixel_res = Vec::new();
                for ((x, y), around, center) in infos {
                    let pixel = calc_life_game_rgb(around, center.to_rgb(), &config);
                    pixel_res.push((x, y, pixel));
                }

                tx_clone
                    .send(pixel_res)
                    .expect("ライフゲームの計算結果の送信に失敗しました。")
            });
        }
        drop(tx);

        for infos in rx {
            for (x, y, pixel) in infos {
                imgbuf.put_pixel(x, y, pixel);
            }
        }

        imgbuf
            .save(get_temp_path(i))
            .expect("画像の保存に失敗しました。");

        prev = image::open(get_temp_path(i)).expect("画像を開けませんでした。");

        //     for (x, y, pixel) in imgbuf.enumerate_pixels_mut() {
        //         let around = get_around(&prev, x, y);
        //         let center = prev.get_pixel(x, y).to_rgb();

        //         *pixel = calc_life_game_rgb(around, center, &life_game_config);
        //         // *pixel = calc_life_game_luma(
        //         //     around,
        //         //     center,
        //         //     &ref_img.get_pixel(x, y).to_rgb(),
        //         //     &life_game_config,
        //         // );

        //         // let color = pixel.channels();
        //         // if max < *color.iter().max().unwrap_or(&0) {
        //         //     max = *color.iter().max().unwrap();
        //         // }

        //         // if min > *color.iter().min().unwrap_or(&255) {
        //         //     min = *color.iter().min().unwrap();
        //         // }
        //     }

        //     // mean_min += min as u32;
        //     // mean_max += max as u32;

        //     // 一枚目の画像でディレクトリがあることを確認しているので、途中で消さない限りnot foundは起こらない。
        //     imgbuf
        //         .save(get_temp_path(i))
        //         .expect("画像の保存に失敗しました。");

        //     prev = image::open(get_temp_path(i)).expect("画像を開けませんでした。");
    }
    println!("\ndone!");
    let end = Instant::now();

    println!("{}", (end - start).as_millis() as f64 / 1000.0);

    // mean_max /= width * height;
    // mean_min /= width * height;

    // // 明るさ補正は前のタイミングですると、ライフゲームに影響が出るのでここで。
    // for i in 0..args.frame {
    //     let progress = ((i + 1) as f64 / args.frame as f64) * 1000.0;
    //     let progress = progress.trunc() / 10.0;
    //     print!(
    //         "optimizing brightness... {:>08}/{:>08} [{:>4.1}%]\r",
    //         i + 1,
    //         args.frame,
    //         progress
    //     );
    //     optimize_img_brightness(get_temp_path(i), mean_max as f64, mean_min as f64);
    // }
    // println!("\ndone!");

    let get_out_path = || {
        let mut new_path = args.output.clone();

        if args.need_uuid {
            new_path.push_str(format!("__{}", Uuid::new_v4()).as_str());
        }

        new_path
    };

    if !matches!(args.file_type, OutputTypeChoices::Frame) {
        println!("encoding mp4...");
        encode_frames_to_mp4(get_out_path().as_str(), "temp/%08d.png", args.fps);
        println!("encoded!");
    }

    if !matches!(args.file_type, OutputTypeChoices::Movie) {
        let new_out_path = get_out_path();
        // 上書き保存できるように、予めフォルダごと削除する。
        if Path::new(new_out_path.as_str()).is_dir() {
            remove_dir_all(new_out_path).expect("ディレクトリを削除できませんでした。");
        }

        rename("temp", get_out_path()).expect("ファイル名の変更に失敗しました。");
    } else {
        println!("removing temp file...");
        remove_dir_all("temp").expect("ディレクトリを削除できませんでした。");
    }
}
