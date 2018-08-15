import java.time.Duration
import java.time.LocalDate
import java.time.LocalDateTime

data class Gigasecond(val origDate: LocalDateTime) {
    constructor(localDate: LocalDate) : this(localDate.atTime(0,0))
    val date = origDate.plusSeconds(1_000_000_000)
}


