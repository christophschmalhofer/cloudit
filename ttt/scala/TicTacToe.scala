//TicTacToe in Konsole

//Build: scalac -deprecation TicTacToe.scala
//Run: scala TicTacToe star

import java.util.Calendar;

  object TicTacToe {
    
    // wer hat in eine Zelle gesetzt
    // wer hat gewonnen
    object who extends Enumeration {
      val Empty = Value("_")
      val Me = Value("o")
      val You = Value("x");
      val Nobody = Value("Niemand");
    }
    
    // falls Spiel entschieden -> Ausstieg aus Eingabeschleife
    // falls Gewinnerposition ermittelt -> Ausstieg aus Suche
    case class GameOverException(
      val player:who.Value, 
      //das ist der ermittelte Zug (direkter oder indirekter Siegeszug)
      val nextMove:Option[(int,int)]) extends Exception

    //Liste von Gewinnerpositionen
    val winnerTriples = List[((Int,Int),(Int,Int),(Int,Int))](
      //Zeilen
      ((0,0),(0,1),(0,2)), 
      ((1,0),(1,1),(1,2)), 
      ((2,0),(2,1),(2,2)),
      //Spalten
      ((0,0),(1,0),(2,0)), 
      ((0,1),(1,1),(2,1)), 
      ((0,2),(1,2),(2,2)),
      //Diagonalen
      ((0,0), (1,1), (2,2)), 
      ((0,2), (1,1), (2,0)))

    // Dient zum Parsen der Konsoleneingabe für Zug in eine Zelle
    // Exctractor: "l,c" => Paar 
    object Cell {
       def unapply(str: String): Option[(Int,Int)] = { 
         val parts = str split ","
         if (parts.length == 2) Some((parts(0).toInt, parts(1).toInt)) else None
       } 
    }

    //Spielfeld: 3x3 Array
    var master = new Array[Array[who.Value]](3,3)


    // Argument "start": Programm macht ersten Zug
    def main(args: Array[String]) {
      println("Du setzt: " + who.You + "\nProgramm setzt: " + who.Me)
      init()
      try {
        var line = ""
        var move = args.length == 1 && args(0) == "start"
        do {
          if(move) {
            meMove()
            checkGameOver()
          }
          render(master)
          line = readLine()
          line match { 
            case Cell(l,c) => 
              if (getWho(master, (l,c)) == who.Empty) { 
                move = true
                setWho(master,(l,c), who.You)
                checkGameOver()
              } else {
                println ("Zeile: " + l + " Spalte: " + c + " nicht leer.")
                move = false
              }
            case _ => println("Ungültige Zelle: " + line); move = false
          }
        } while (line != "")
      } catch {
        case ex: GameOverException => println( ex.player + " hat gewonnen");render(master)
      }
    }
    
    // Siegstellung oder Remis => GameOverException
    def checkGameOver() {
      checkIsWinner(None,master, (0,0))
      if (emptyCells(master).isEmpty) {
        throw new GameOverException(who.Nobody, None)
      }
    }

    // player hat gewonnen => GameOverException
    // player = None => hat jemand gewonnen?
    def checkIsWinner(player:Option[who.Value], field:Array[Array[who.Value]], nextMove:(Int,Int)) {
    
      def isWinner(triple:((Int,Int),(Int,Int),(Int,Int))):Boolean = {
        getWho(field, triple._1) != who.Empty  && getWho(field, triple._2) == getWho(field, triple._1) && 
        getWho(field, triple._3) == getWho(field,triple._1) && 
        (player.isEmpty || getWho( field,triple._1) == player.get())
      }
      
      winnerTriples.find(isWinner(_)) match {
        case Some(triple) => throw new GameOverException(getWho(field, triple._1), Some(nextMove))
        case None => {}
      }
    }

    // Programm zieht
    def meMove():Unit = {
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
    
    // liefert Gegner von player
    def getOtherPlayer(player:who.Value):who.Value = {
      if (player.equals(who.Me)) who.You else who.Me
    }

    // player setzt in dem geklonten Spielstand in die Zelle cell 
    def cloneAndMoveAndCheckIsWinner(field:Array[Array[who.Value]], player:who.Value, cell:(Int,Int)) { 
      val clone = cloneField(field)
      setWho(clone, cell, player)
      checkIsWinner(Some(player), clone, cell)
    }
    
    // das ist ein Min-Max inspirierter Algorithmus
    // die Minumum Ermittlung ist aber naiv, man hat also eine Chance (wenn man selbst anfängt)
    def searchWinner(player:who.Value, field:Array[Array[who.Value]]) {
      // zuerst einfachen Siegeszug suchen
      emptyCells(field).foreach(cloneAndMoveAndCheckIsWinner(field, player, _))

      // Abbruchbedingung: falls Gegner einfachen Siegeszug hat -> hilft kein indirekter
      try {
        emptyCells(field).foreach(cloneAndMoveAndCheckIsWinner(field, getOtherPlayer(player), _))
      } catch {
        case ex: GameOverException => {return}
      }

      // Abbruchbedingung: kann indirekten Siegeszug nur geben, wenn noch mindestens drei Felder leer
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
          setWho(cloneClone, emptyCell, getOtherPlayer(player))
          try {
            searchWinner(player, cloneClone)
            alwaysWinner = false;
          } catch {
            case ex: GameOverException => {}
            //alwaysWinner bleibt true;
          }
        }
        if (alwaysWinner) {
          //indirekten Siegeszug gefunden
          throw new GameOverException(player, Some(myCell))
        }
      }
    }
    
    // liefert eine geklontes Spielfeld (Besetzung identisch zu Parameter field)
    def cloneField(field:Array[Array[who.Value]]):Array[Array[who.Value]] = { 
      // funktioniert tatsächlich
      return field.map( l => l.map( v => v))
    }

    // zeigt Spielfeld an
    def render(field:Array[Array[who.Value]]) {
      for(l <- field) {
        for (c <- l) {
          print(c + "\t")
        }
        println()
      }
    }
    
    // welcher player hat in die Zelle t des Feldes gesetzt
    def getWho(field:Array[Array[who.Value]], t:(Int,Int)): who.Value = {
      field(t._1)(t._2)
    }

    // player setzt in Zelle t
    def setWho(field:Array[Array[who.Value]], t:(Int,Int), player:who.Value) {
      field(t._1)(t._2) = player
    }

    // Spielfeld initialisieren
    def init() {
      //Comprehension ist verständlicher
      //master.elements.foreach( l => l.indices.elements.foreach( l.update(_, who.Empty)))
      for(l <- master; i <- 0 until l.length) {
        l(i) = who.Empty
      }
    }
    
    // liefert freie Zellen
    def emptyCells(field:Array[Array[who.Value]]):Seq[(Int,Int)] = {
      for (
        l <- 0 to 2; 
        c <- 0 to 2
        if who.Empty.equals(field(l)(c))
      ) yield(l,c)
    }

  }
