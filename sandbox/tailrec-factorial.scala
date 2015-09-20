#!/usr/bin/scala
!#

import scala.annotation.tailrec

def factorial1(n: Int): Int = if (n == 0) 1 else n * factorial1(n - 1)

def factorial2(n: Int): Int = {
    @tailrec def fact(f: Int, m: Int): Int = if (m <= 1) f else fact(m * f, m - 1)
    fact(n, n - 1)
}

def test(fact: (Int) => Int) = {
    val t1 = System.currentTimeMillis()
    for (x <- 1 to 1000 * 1000 * 1000) {
        val b = fact(16)
    }
    val t2 = System.currentTimeMillis()
    println(t2 - t1)
}

if (args.length > 0 && args(0) == "tailrec") {
    for (x <- 1 to 10) test(factorial2)
} else {
    for (x <- 1 to 10) test(factorial1)
}

