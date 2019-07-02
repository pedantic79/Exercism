use unicode_segmentation::UnicodeSegmentation;

pub fn reverse(input: &str) -> String {
    input.graphemes(true).rev().collect()
}

mod tests {
    #[test]
    fn rosetta() {
        assert_eq!(super::reverse("as⃝df̅"), "f̅ds⃝a");
    }
}
