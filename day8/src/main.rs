
fn main() {
    let input = include_bytes!("../input");
    const WIDTH: usize = 25;
    const HEIGHT: usize = 6;

    let answer = input
        .chunks_exact(WIDTH * HEIGHT)
        .map(|layer| {
            let mut zeros = 0;
            let mut ones  = 0;
            let mut twos  = 0;
            for digit in layer.iter() {
                match *digit {
                    b'0' => { zeros += 1; }
                    b'1' => { ones  += 1; }
                    b'2' => { twos  += 1; }
                    _    => { }
                }
            }
            (zeros, ones, twos)
        })
        .min_by_key(|(zeros, _, _)| *zeros)
        .map(|(_, ones, twos)| ones * twos)
        .unwrap();

    println!("part 1: {}", answer);

    let image = input
        .chunks_exact(WIDTH * HEIGHT)
        .fold(
            Vec::<u8>::new(),
            |mut composed, layer| {
                if composed.is_empty() {
                    composed = layer.iter().copied().collect();
                }
                else {
                    for (dest, src) in composed.iter_mut().zip(layer.iter()) {
                        if *dest == b'2' {
                            *dest = *src;
                        }
                    }
                }
                composed
            }
        )
        .chunks_exact(WIDTH)
        .for_each(|mut row| {
            let line: String = row.iter().map(|p|
                match p {
                    b'0' => ' ',
                    b'1' => '#',
                    b'2' => '/',
                    _    => '?'
                }
            ).collect();
            println!("{}", line);
        });
}

