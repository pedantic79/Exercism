#[derive(Debug)]
pub enum Pitch {
    Sharp,
    Flat,
}

impl Pitch {
    pub fn new(tonic: &str) -> Self {
        // Technically C-major and a-minor have no flats, but C-major chromatic needs flats
        const SHARP_TONICS: [&str; 14] = [
            "C", "a", "G", "D", "A", "E", "B", "F#", "e", "b", "f#", "c#", "g#", "d#",
        ];

        if SHARP_TONICS.contains(&tonic) {
            Pitch::Sharp
        } else {
            Pitch::Flat
        }
    }
}
