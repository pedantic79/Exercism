package tournament

import (
	"bufio"
	"fmt"
	"io"
	"sort"
	"strings"
)

type Record struct {
	win  int
	loss int
	draw int
}

type Results struct {
	name   string
	points int
}

type ResultsSlice []Results

func (r ResultsSlice) Len() int {
	return len(r)
}

func (r ResultsSlice) Less(i, j int) bool {
	if r[i].points == r[j].points {
		return r[i].name < r[j].name
	}
	return r[i].points > r[j].points
}

func (r ResultsSlice) Swap(i, j int) {
	r[i], r[j] = r[j], r[i]
}

func Tally(input io.Reader, output io.Writer) error {
	teamRecord := make(map[string]*Record)

	rd := bufio.NewReader(input)
	eof := false

	for !eof {
		line, err := rd.ReadString('\n')
		if err != nil && err == io.EOF {
			eof = true
		}

		line = strings.TrimSuffix(line, "\n")
		teams := strings.Split(line, ";")

		// Somes we get a blank line
		if len(teams) == 1 {
			continue
		}

		if len(teams) == 2 {
			return fmt.Errorf("Parse error parsing, %s", line)
		}

		if _, ok := teamRecord[teams[0]]; !ok {
			teamRecord[teams[0]] = &Record{}
		}

		if _, ok := teamRecord[teams[1]]; !ok {
			teamRecord[teams[1]] = &Record{}
		}

		if err := tallyOutcome(teamRecord, teams); err != nil {
			return err
		}
	}

	results := make([]Results, 0, len(teamRecord))
	for name, record := range teamRecord {
		results = append(results, Results{
			name:   name,
			points: record.win*3 + record.draw,
		})
	}

	sort.Sort(ResultsSlice(results))

	printScore(output, results, teamRecord)
	return nil
}

func tallyOutcome(rec map[string]*Record, fields []string) error {
	switch fields[2] {
	case "win":
		rec[fields[0]].win++
		rec[fields[1]].loss++
	case "loss":
		rec[fields[0]].loss++
		rec[fields[1]].win++
	case "draw":
		rec[fields[0]].draw++
		rec[fields[1]].draw++
	default:
		return fmt.Errorf("Unknown outcome %v", fields[2])
	}
	return nil
}

func printScore(w io.Writer, points []Results, rec map[string]*Record) {
	fmt.Fprintln(w, "Team                           | MP |  W |  D |  L |  P")
	for _, results := range points {
		fmt.Fprintf(w, "%-31s| %2d | %2d | %2d | %2d | %2d\n",
			results.name,
			rec[results.name].win+rec[results.name].loss+rec[results.name].draw,
			rec[results.name].win,
			rec[results.name].draw,
			rec[results.name].loss,
			results.points,
		)
	}
}
