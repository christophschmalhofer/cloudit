
import java.util.Calendar;

  object TicTacToe {

    object who extends Enumeration {
      val Empty = Value("Empty")
      val Me = Value("Me")
      val You = Value("You");
      val Nobody = Value("Nobody");
    }

    case class GameOverException(val player:who.Value, val field:Array[Array[who.Value]], val nextMove:Option[(int,int)]) extends Exception

    val triples = List[List[(Int,Int)]](
      //Zeilen
      List((0,0),(0,1),(0,2)), 
      List((1,0),(1,1),(1,2)), 
      List((2,0),(2,1),(2,2)),
      //Spalten
      List((0,0),(1,0),(2,0)), 
      List((0,1),(1,1),(2,1)), 
      List((0,2),(1,2),(2,2)),
      //Diagonalen
      List((0,0), (1,1), (2,2)), 
      List((0,2), (1,1), (2,0)))

    object Position {
       def unapply(str: String): Option[(Int,Int)] = { 
         val parts = str split ","
         if (parts.length == 2) Some((parts(0).toInt, parts(1).toInt)) else None
       } 
    }

    var master = new Array[Array[who.Value]](3,3)


    def main(args: Array[String]) {
      init()
      try {
        var line = ""
        do {
          move()
          render(master)
          checkGameOver(master, (0,0))
          if (emptyCells(master).isEmpty) {
            throw new GameOverException(who.Nobody, master, None)
          }
          var inputOk = false
          line = readLine()
          line match { 
            case Position(l,c) => 
              if (getWho(master, (l,c)) == who.Empty) { 
                setWho(master,(l,c), who.You)
                inputOk = true
              } else {
                println ("Line: " + l + " Column: " + c + " nicht leer.")
              }
            case _ => println("Ungültige Position: " + line) 
          }
          if(inputOk) {
            checkGameOver(master, (0,0))
            if (emptyCells(master).isEmpty) {
              throw new GameOverException(who.Nobody, master, None)
            }
          }
        } while (line != "")
      } catch {
        case ex: GameOverException => println( ex.player + " hat gewonnen");render(master)
      }
    }

    def checkGameOver(pfield:Array[Array[who.Value]], nextMove:(Int,Int)) {
      for(atriple <- triples) {
        val (b,v) = checkTriple( pfield, atriple(0), atriple(1), atriple(2))
        if (b) { 
          println( "GameOver at: " + atriple)
          throw new GameOverException(v, pfield, Some(nextMove))
        }
      }
    }

    def checkIsWinner(player:who.Value, pfield:Array[Array[who.Value]], nextMove:(Int,Int)) {
      def isWinner((Int, Int), second:(Int,Int), third:(Int,Int)): (Boolean, who.Value)= {
      (!getWho(pfield, first).equals(who.Empty) && getWho(pfield, second).equals(getWho(pfield, first)) && getWho(pfield, third).equals(getWho(pfield,first)), getWho(pfield,first))
    }
 
      for(atriple <- triples) {
        val (b,v) = checkTriple( pfield, atriple(0), atriple(1), atriple(2))
        if (b && getWho( pfield,atriple(0)).equals(player)) { 
          throw new GameOverException(v, pfield, Some(nextMove))
        }
      }
    }


    def move():Unit = {
      // habe ich einen Siegeszug ?
      try {
          searchWinner(who.Me, master)
      } catch {
        case ex: GameOverException => val move = ex.nextMove.getOrElse(emptyCells(master).first); setWho(master, move, who.Me);return 
      }

      // hast du einen Siegeszug ?
      try {
          searchWinner(who.You, master)
      } catch {
        //vermassle seinen Siegeszug 
        case ex: GameOverException => setWho(master, ex.nextMove.getOrElse(emptyCells(master).first), who.Me);return
      }
      val emptyCellsV =  emptyCells(master)
      val rand = new Random(Calendar.getInstance().getTimeInMillis())
      setWho(master,emptyCellsV(rand.nextInt(emptyCellsV.length)), who.Me)    
    }
    
    def getOther(player:who.Value):who.Value = {
      if (player.equals(who.Me)) who.You else who.Me
    }
    
    def searchWinner(player:who.Value, field:Array[Array[who.Value]]) {
      // zuerst einfachen Siegeszug suchen
      for( emptyCell <-emptyCells(field)) {
        val clone = cloneField(field)
        setWho(clone, emptyCell, player)
        checkIsWinner(player, clone, emptyCell);
      }
      // falls Gegner einfachen Siegeszug hat -> hilt kein indirekter
      for( emptyCell <-emptyCells(field)) {
        val clone = cloneField(field)
        setWho(clone, emptyCell, getOther(player))
        try {
          checkIsWinner(getOther(player), clone, emptyCell);
        } catch {
          case ex: GameOverException => {return}
        }
      }

      // kann indirekten Siegeszug nur geben, wenn noch mindestens drei Felder leer
      if (emptyCells(field).length <= 3) {
        return;
      }

      // indirekten Siegeszug suchen 
      for( myCell <-emptyCells(field)) {
        val clone = cloneField(field)
        setWho(clone, myCell, player)
        // gibt es für jeden gegnerischen Zug einen direkten Siegerzug
        // oder indirekten Siegerzug (mit Einschränkung: kein kürzerer Siegeszug des Gegners) 
        // handelt es sich um einen indirekten
        // Siegerzug
        var alwaysWinner = true
        for( emptyCell <-emptyCells(clone)) {
          val cloneClone = cloneField(clone)
          setWho(cloneClone, emptyCell, getOther(player))
          try {
            searchWinner(player, cloneClone)
            alwaysWinner = false;
          } catch {
            case ex: GameOverException => {}
            //allWinner bleibt true;
          }
        }
        if (alwaysWinner) {
          //indirekten Siegeszug gefunden
          throw new GameOverException(player, clone, Some(myCell))
        }
      }
    }
    
    def cloneField(pfield:Array[Array[who.Value]]):Array[Array[who.Value]] = { 
      // funktioniert tatsächlich
      return pfield.map( l => l.map( v => v))
    }

    def render(field:Array[Array[who.Value]]) {
      for(l <- field) {
        for (c <- l) {
          print(c + "\t")
        }
        println()
      }
    }

      
    def checkTriple(pfield:Array[Array[who.Value]], first:(Int, Int), second:(Int,Int), third:(Int,Int)): (Boolean, who.Value)= {
      (!getWho(pfield, first).equals(who.Empty) && getWho(pfield, second).equals(getWho(pfield, first)) && getWho(pfield, third).equals(getWho(pfield,first)), getWho(pfield,first))
    }


    def getWho(field:Array[Array[who.Value]], t:(Int,Int)): who.Value = {
      field(t._1)(t._2)
    }

    def setWho(field:Array[Array[who.Value]], t:(Int,Int), v:who.Value){
      field(t._1)(t._2) = v
    }

    def init() {
      for(l <- master; i <- 0 until l.length) {
        l(i) = who.Empty
      }
    }

    def emptyCells(pfield:Array[Array[who.Value]]):Seq[(Int,Int)] = {
      for (
        l <- 0 to 2; 
        c <- 0 to 2
        if who.Empty.equals(pfield(l)(c))
      ) yield(l,c)
    }
  }
