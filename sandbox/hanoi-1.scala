#!/bin/sh
exec scala "$0" "$@"
!#

import scala.collection.mutable.ArrayStack
import scala.collection.mutable.Set

class Tower(val floors: ArrayStack[Int]) {

    def height = floors.size

    def accepts(from: Tower) = {
        from.floors.size > 0 && (floors.size == 0 || from.floors.top < floors.top)
    }

    override def toString() = toStrings(24).mkString("\n")

    def floorSizeToString(s: Int, width: Int) = {
        val side = width / 2 - s
        (" " * side) + ("-" * (s * 2)) + (" " * side)
    }

    def floorToString(f: Int, width: Int) = {
        if(f < floors.size) floorSizeToString(floors(floors.size - f - 1), width) else (" " * width)
    }

    def toStrings(width: Int) = {
        floors.map(f => floorSizeToString(f, width))
    }

}

object Tower {

    def foundation() = new Tower(new ArrayStack)

    def complete(bottom: Int) = range(bottom, 1)

    private def range(bottom: Int, top: Int) = new Tower(ArrayStack.range(top, bottom + 1))

}

class City(val towers: Array[Tower]) {

    def size = towers.size

    override def toString() = {
        val cityHeight = 12
        // from top to bottom
        (for(level <- cityHeight - 1 to 0 by -1) yield {
            (for(tower <- towers) yield {
                tower.floorToString(level, 24)
            }).mkString(" ")
        }).mkString("\n") +"\n" + "^" * (24 * 3 + 2)
    }

}

object City {

    def apply(towers: Tower*) = new City(towers.toArray)

}

object Crane {

    def move(city: City, from: Int, to: Int) {
        val fromTower = city.towers(from)
        val toTower = city.towers(to)
        if(toTower accepts fromTower) {
            toTower.floors push fromTower.floors.pop
        } else {
            println("Beep! Tower " + (to + 1) + " cannot receive the top of tower " + (from + 1))
        }
    }

}

object Move {

    def superMove(height: Int, from: Int, to: Int, move: (Int, Int) => Unit): () => Unit = {
        height match {
            case 0 => () => ()
            case 1 => () => move(from, to)
            case _ => {
                val s = Set(0, 1, 2)
                s -= from
                s -= to
                () => {
                    superMove(height - 1, from, s.head, move)()
                    move(from, to)
                    superMove(height - 1, s.head, to, move)()
                }
            }
        }
    }

}

def play {

    println("Building initial city...")

    val tower1 = Tower.complete(9)
    val tower2 = Tower.foundation
    val tower3 = Tower.foundation

    val city = City(tower1, tower2, tower3)

    println(city)

    var success = false
    var moves = 0
    println("Crane is on the move...")
    val go = Move.superMove(city.towers(0).height, 0, 2,
        (from: Int, to: Int) => {
            Crane move(city, from, to)
            println(city)
            moves += 1
            if(city.towers(0).height == 0 && city.towers(1).height == 0) {
                success = true
            }
        })

    go()

    println(
        if(success)
            "Clap clap clap! You are a certified Scala pig! :-) (" + moves + ")"
        else
            "Pffffff :-(( (" + moves + ")"
    )

}

play
