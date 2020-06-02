class Luhn:
    def __init__(self, card_num: str):
        self._card_num = list(reversed(card_num.replace(" ", "")))

    def valid(self) -> bool:
        try:
            total = sum(
                Luhn.double_if_second(index, character)
                for (index, character) in enumerate(self._card_num)
            )
        except ValueError:
            return False

        return len(self._card_num) > 1 and total % 10 == 0

    def double_if_second(index: int, character: str) -> int:
        number = int(character)

        if index % 2 == 1:
            number *= 2
            if number > 9:
                number -= 9

        return number
