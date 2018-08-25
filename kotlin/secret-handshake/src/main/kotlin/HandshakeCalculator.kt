object HandshakeCalculator {
    val signals = arrayOf(Signal.WINK, Signal.DOUBLE_BLINK, Signal.CLOSE_YOUR_EYES, Signal.JUMP)

    fun calculateHandshake(secretMessage: Int): List<Signal> {
        var signalList = mutableListOf<Signal>()
        var binaryMessage = secretMessage

        for (s in signals) {
            if (binaryMessage and 1 == 1)  {
                signalList.add(s)
            }
            binaryMessage = binaryMessage shr 1
        }

        if (binaryMessage == 1) {
            signalList.reverse()
        }

        return signalList
    }
}
