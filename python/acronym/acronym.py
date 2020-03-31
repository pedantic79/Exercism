def normalize_input(input_str):
    for (character, replacement) in [("-", " "), ("_", "")]:
        input_str = input_str.replace(character, replacement)
    return input_str


def abbreviate(words):
    return "".join(
        word[0].upper()
        for word in normalize_input(words).split(" ")
        if word != ""
    )
