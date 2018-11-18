package erratum

func Use(o ResourceOpener, input string) (err error) {
	var resource Resource

	for {
		if resource, err = o(); err == nil {
			break
		}

		if _, ok := err.(TransientError); !ok {
			return err
		}
	}

	defer resource.Close()
	defer func() {
		if r := recover(); r != nil {
			if frobError, ok := r.(FrobError); ok {
				resource.Defrob(frobError.defrobTag)
			}
			err = r.(error)
		}
	}()

	resource.Frob(input)

	return err
}
