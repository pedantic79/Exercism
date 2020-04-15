def normalize_input(input_str):
    return input_str.replace("-", " ").replace("_", "")


def abbreviate(words):
    return "".join(
        word[0].upper() for word in normalize_input(words).split(" ") if word
    )
