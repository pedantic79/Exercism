use unicode_segmentation::UnicodeSegmentation;

pub fn reverse(input: &str) -> String {
    input.graphemes(true).rev().collect()
}

#[cfg(test)]
mod tests {
    #[test]
    fn rosetta() {
        assert_eq!(super::reverse("asβdfΜ"), "fΜdsβa");
    }

    #[test]
    fn emoji() {
        // This test fails with unicode-segmentaton <1.4.0
        assert_eq!(super::reverse("π€¦πΌπͺ"), "πͺπ€¦πΌ");
    }

    #[test]
    fn emoji12() {
        // This emoji is one grapheme, so the reverse should be the same
        // https://emojipedia.org/factory-worker-medium-skin-tone/
        assert_eq!(super::reverse("π§π½βπ­"), "π§π½βπ­")
    }
}
