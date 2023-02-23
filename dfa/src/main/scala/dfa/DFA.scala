package dfa // leave this line in the file

case class State(label: String)

case class Transition(from: State, to: State, symbol: Char)

class DFA(val states: Set[State], val transitions: Set[Transition],
    val start: State, val accept: Set[State]):
    
    def accepts(input: String): Boolean = {
        var currentState = start

        for (c <- input) {
            val currTemp = (transitions.filter(_.from == currentState).filter(_.symbol == c)).head.to
            currentState = currTemp
        }

        accept.contains(currentState)
    }