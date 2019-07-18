package flatten

func Flatten(input interface{}) []interface{} {
	output := make([]interface{}, 0)

	for _, value := range input.([]interface{}) {
		if value == nil {
			continue
		}

		if _, ok := value.([]interface{}); ok {
			output = append(output, Flatten(value)...)
		} else {
			output = append(output, value)
		}
	}
	return output
}
