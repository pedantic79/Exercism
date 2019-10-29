// unicode-segmentation does not properly handle the most modern emoji
// specifically the ZWJ. So I'm using unic_segment which does.
// use unicode_segmentation::UnicodeSegmentation;
use unic_segment::Graphemes;

pub fn reverse(input: &str) -> String {
    // input.graphemes(true).rev().collect()
    Graphemes::new(input).rev().collect()
}

#[cfg(test)]
mod tests {
    #[test]
    fn rosetta() {
        assert_eq!(super::reverse("as⃝df̅"), "f̅ds⃝a");
    }

    #[test]
    fn emoji() {
        assert_eq!(super::reverse("🤦🏼👪"), "👪🤦🏼");
    }

    #[test]
    fn emoji12() {
        // This emoji is one grapheme, so the reverse should be the same
        // https://emojipedia.org/factory-worker-medium-skin-tone/
        assert_eq!(super::reverse("🧑🏽‍🏭"), "🧑🏽‍🏭")
    }
}
