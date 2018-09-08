package rotationalcipher

import (
	"unicode"
)

func RotationalCipher(plain string, key int) string {
	var cipher []rune
	for _, ch := range plain {
		cipher = append(cipher, rotate(ch, key))
	}

	return string(cipher)
}

func rotate(plain rune, key int) rune {
	if unicode.IsLetter(plain) {
		a := 'a'
		if unicode.IsUpper(plain) {
			a = 'A'
		}

		return rune(int(plain-a)+key)%26 + a
	}

	return plain
}
