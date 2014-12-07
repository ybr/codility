package ybr


object FrogJmp {
    def solution(X: Int, Y: Int, D: Int): Int = Math.ceil(Math.abs(X - Y) / D.toDouble).toInt
}