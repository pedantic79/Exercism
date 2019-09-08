use std::{convert::TryFrom, fmt, ops::AddAssign};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Note(u8);

type Error = super::Error;

impl TryFrom<&str> for Note {
    type Error = Error;

    fn try_from(note: &str) -> Result<Self, Self::Error> {
        let note_vec = note.as_bytes();
        match note.len() {
            1 => Note::new(note_vec[0], None),
            2 => Note::new(note_vec[0], Some(note_vec[1])),
            _ => Err(Error::NoteConversionLengthError),
        }
    }
}

impl AddAssign<u8> for Note {
    fn add_assign(&mut self, other: u8) {
        self.0 = (self.0 + other) % 12;
    }
}

impl fmt::Display for Note {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match (self.0, f.alternate()) {
            (0, _) => "A",
            (1, true) => "A#",
            (1, false) => "Bb",
            (2, _) => "B",
            (3, _) => "C",
            (4, true) => "C#",
            (4, false) => "Db",
            (5, _) => "D",
            (6, true) => "D#",
            (6, false) => "Eb",
            (7, _) => "E",
            (8, _) => "F",
            (9, true) => "F#",
            (9, false) => "Gb",
            (10, _) => "G",
            (11, true) => "G#",
            (11, false) => "Ab",
            _ => unreachable!(),
        };

        f.write_str(s)
    }
}

impl Note {
    fn new(name: u8, pitch: Option<u8>) -> Result<Self, Error> {
        const SHARP: Option<u8> = Some(b'#');
        const FLAT: Option<u8> = Some(b'b');

        match (name.to_ascii_uppercase() as char, pitch) {
            ('A', None) => Ok(Note(0)),
            ('A', SHARP) | ('B', FLAT) => Ok(Note(1)),
            ('B', None) => Ok(Note(2)),
            ('C', None) => Ok(Note(3)),
            ('C', SHARP) | ('D', FLAT) => Ok(Note(4)),
            ('D', None) => Ok(Note(5)),
            ('D', SHARP) | ('E', FLAT) => Ok(Note(6)),
            ('E', None) => Ok(Note(7)),
            ('F', None) => Ok(Note(8)),
            ('F', SHARP) | ('G', FLAT) => Ok(Note(9)),
            ('G', None) => Ok(Note(10)),
            ('G', SHARP) | ('A', FLAT) => Ok(Note(11)),
            _ => Err(Error::NoteConversionInvalidError),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::convert::TryInto;

    #[test]
    fn major() {
        let name = ["A", "B", "C", "D", "E", "F", "G"];
        let value = [0, 2, 3, 5, 7, 8, 10];
        for (&n, &v) in name.iter().zip(value.iter()) {
            let note = Note::try_from(n).unwrap();
            assert_eq!(note, Note(v));
            assert_eq!(note.to_string(), n);
        }
    }

    #[test]
    fn pitch() {
        let name = ["A#", "Bb", "C#", "Db", "D#", "Eb", "F#", "Gb", "G#", "Ab"];
        let value = [1, 1, 4, 4, 6, 6, 9, 9, 11, 11];

        for (&n, &v) in name.iter().zip(value.iter()) {
            let note = Note::try_from(n).unwrap();
            assert_eq!(note, Note(v));

            // We default to flats, use {:#} to get the sharp value
            if n.ends_with('b') {
                assert_eq!(note.to_string(), n);
            } else {
                assert_eq!(format!("{:#}", note), n)
            }
        }
    }

    #[test]
    fn panic_wrong_case() {
        let n: Result<Note, Error> = "aB".try_into();
        assert_eq!(n, Err(Error::NoteConversionInvalidError));
    }

    #[test]
    fn name() {
        let n: Result<Note, Error> = "Cb".try_into();
        assert_eq!(n, Err(Error::NoteConversionInvalidError));
    }

    #[test]
    fn panic_invalid_note2() {
        assert_eq!(Note::try_from("E#"), Err(Error::NoteConversionInvalidError));
    }

    #[test]
    fn panic_invalid_length() {
        assert_eq!(Note::try_from("Ebb"), Err(Error::NoteConversionLengthError));
    }
}
