mod note;
mod pitch;

use note::Note;
use pitch::Pitch;
use std::{convert::TryFrom, str};

#[derive(Debug, PartialEq)]
pub enum Error {
    NoteConversionLengthError,
    NoteConversionInvalidError,
    IntervalsConversionError(char),
}

pub struct Scale {
    pitch: Pitch,
    notes: Vec<Note>,
}

impl Scale {
    pub fn new(tonic: &str, intervals: &str) -> Result<Self, Error> {
        let mut start = Note::try_from(tonic)?;
        let mut notes = Vec::with_capacity(intervals.len());

        for step in intervals.chars() {
            notes.push(start);

            start += match step {
                'm' => 1,
                'M' => 2,
                'A' => 3,
                _ => return Err(Error::IntervalsConversionError(step)),
            };
        }

        Ok(Self {
            pitch: Pitch::new(tonic),
            notes,
        })
    }

    pub fn chromatic(tonic: &str) -> Result<Self, Error> {
        Self::new(tonic, str::from_utf8(&[b'm'; 12]).unwrap())
    }

    pub fn enumerate(&self) -> Vec<String> {
        let format: Box<dyn Fn(&Note) -> String> = match self.pitch {
            Pitch::Sharp => Box::new(|note: &Note| format!("{:#}", note)),
            Pitch::Flat => Box::new(|note: &Note| note.to_string()),
        };

        self.notes.iter().map(|n| format(n)).collect()
    }
}
