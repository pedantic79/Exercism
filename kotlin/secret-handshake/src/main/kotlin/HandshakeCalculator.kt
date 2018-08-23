object HandshakeCalculator {
    val signals = arrayOf(Signal.WINK, Signal.DOUBLE_BLINK, Signal.CLOSE_YOUR_EYES, Signal.JUMP)

    fun calculateHandshake(num: Int): List<Signal> {
        var ret = mutableListOf<Signal>()
        var n = num

        for (s in signals) {
            if (n and 1 == 1)  {
                ret.add(s)
            }
            n = n shr 1
        }

        if (n == 1) {
            ret.reverse()
        }

        return ret
    }
}