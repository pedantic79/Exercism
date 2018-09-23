package twelve

import (
	"fmt"
	"strings"
)

var gifts = []string{
	"a Partridge in a Pear Tree",
	"two Turtle Doves",
	"three French Hens",
	"four Calling Birds",
	"five Gold Rings",
	"six Geese-a-Laying",
	"seven Swans-a-Swimming",
	"eight Maids-a-Milking",
	"nine Ladies Dancing",
	"ten Lords-a-Leaping",
	"eleven Pipers Piping",
	"twelve Drummers Drumming",
}

var ordinal = []string{
	"first",
	"second",
	"third",
	"fourth",
	"fifth",
	"sixth",
	"seventh",
	"eighth",
	"ninth",
	"tenth",
	"eleventh",
	"twelfth",
}

func giftVerses(days int) string {
	verse := make([]string, days)

	for d := 0; d < days; d++ {
		verse[d] = gifts[days-d-1]
	}

	if days > 1 {
		verse[days-1] = "and " + verse[days-1]
	}

	return strings.Join(verse, ", ")
}

func Verse(days int) string {
	return fmt.Sprintf("On the %s day of Christmas my true love gave to me, %s.",
		ordinal[days-1],
		giftVerses(days))
}

func Song() string {
	days := 12

	verse := make([]string, days)
	for d := 0; d < days; d++ {
		verse[d] = Verse(d+1) + "\n"
	}

	return strings.Join(verse, "")
}
