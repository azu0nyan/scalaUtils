package utils.math.planar.algo.polygonClipping

import utils.datastructures.containers.DoubleLinkedList.DoubleLinkedList
import utils.math.planar.V2

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

//
// converts a list of segments into a list of regions, while also removing unnecessary verticies
//
object Chainer {
  def apply(segments: Iterable[Segment])(implicit epsilon: Epsilon): Iterable[Region] = {
    var chains: ListBuffer[DoubleLinkedList[V2]] = new ListBuffer[DoubleLinkedList[V2]]
    var regions: ListBuffer[Region] = new ListBuffer[Region]

    segments.foreach(seg => {
      var pt1 = seg.start
      var pt2 = seg.end
      if (EpsilonOps.pointsSame(pt1, pt2)) {
        log.warning("Warning: Zero-length segment detected your epsilon is probably too small or too large")
        // return
      } else {
        //      if (buildLog)
        //        buildLog.chainStart(seg)

        // search for two chains that this segment matches
        case class Match(var index: Int, var matches_head: Boolean, var matches_pt1: Boolean)

        var first_match:Match = null//new Match(0, false, false)
        var second_match:Match = null//new Match(0, false, false)
        var matches:Int = 0
//        var next_match = first_match

        def setMatch(m:Match): Boolean = {
          // return true if we've matched twice
          if(matches == 0  ){
            matches = 1
            first_match = m
            return false
          } else if(/*matches == 1 && */first_match == m){
            return false
          }else {
            matches = 2
            second_match = m
            return true
          }
//          next_match.index = index
//          next_match.matches_head = matches_head
//          next_match.matches_pt1 = matches_pt1
//          if (next_match == first_match) {
//            next_match = second_match
//            return false
//          }
//          next_match = null
//          return true // we've matched twice, we're done here
        }

        import util.control.Breaks._
        breakable {
          for (i <- chains.indices) {
            val chain = chains(i)
            val head = chain.first
            var head2 = chain.second
            val tail = chain.last
            var tail2 = chain.secondLast
            if (EpsilonOps.pointsSame(head, pt1)) {
              if (setMatch(Match(i, true, true)))
                break()
            } else if (EpsilonOps.pointsSame(head, pt2)) {
              if (setMatch(Match(i, true, false)))
                break()
            } else if (EpsilonOps.pointsSame(tail, pt1)) {
              if (setMatch(Match(i, false, true)))
                break()
            } else if (EpsilonOps.pointsSame(tail, pt2)) {
              if (setMatch(Match(i, false, false)))
                break()
            }
          }
        }
        if (/*next_match == first_match*/matches == 0) {
          // we didn't match anything, so create a new chain
          chains += {
            val r = new DoubleLinkedList[V2]()
            r.addFirst(pt1)
            r.addLast(pt2)
            r
          }
          //        if (buildLog)
          //          buildLog.chainNew(pt1, pt2)
          // return //todo
        } else if (matches == 1 ) {
          // we matched a single chain

          //        if (buildLog)
          //          buildLog.chainMatch(first_match.index)

          // add the other point to the apporpriate end, and check to see if we've closed the
          // chain into a loop

          val index = first_match.index
          val pt = if (first_match.matches_pt1) pt2 else pt1 // if we matched pt1, then we add pt2, etc
          val addToHead = first_match.matches_head // if we matched at head, then add to the head

          val chain = chains(index)
          var grow = if (addToHead) chain.first else chain.last
          val grow2 = if (addToHead) chain.second else chain.secondLast
          val oppo = if (addToHead) chain.last else chain.first
          val oppo2 = if (addToHead) chain.secondLast else chain.second

          if (EpsilonOps.pointsCollinear(grow2, grow, pt)) {
            // grow isn't needed because it's directly between grow2 and pt:
            // grow2 ---grow---> pt
            if (addToHead) {
              //            if (buildLog)              buildLog.chainRemoveHead(first_match.index, pt)
              chain.removeFirst() //shift
            } else {
              //            if (buildLog)     buildLog.chainRemoveTail(first_match.index, pt)
              chain.removeLast() //pop
            }
            grow = grow2 // old grow is gone... new grow is what grow2 was
          }

          if (EpsilonOps.pointsSame(oppo, pt)) {
            // we're closing the loop, so remove chain from chains
            chains.remove(index)

            if (EpsilonOps.pointsCollinear(oppo2, oppo, grow)) {
              // oppo isn't needed because it's directly between oppo2 and grow:
              // oppo2 ---oppo--->grow
              if (addToHead) {
                //              if (buildLog)               buildLog.chainRemoveTail(first_match.index, grow)
                chain.removeLast() //pop()
              } else {
                //              if (buildLog)             buildLog.chainRemoveHead(first_match.index, grow)
                chain.removeFirst() //shift()
              }
            }

            //          if (buildLog)        buildLog.chainClose(first_match.index)

            // we have a closed chain!
            regions += chain.bakeToSeq
            // return
          } else {
            // not closing a loop, so just add it to the apporpriate side
            if (addToHead) {
              //          if (buildLog)     buildLog.chainAddHead(first_match.index, pt)
              chain.addFirst(pt)
            } else {
              //          if (buildLog)            buildLog.chainAddTail(first_match.index, pt)
              chain.addLast(pt)
            }
            //  return //todo
          }
        } else {//matches == 2
          // otherwise, we matched two chains, so we need to combine those chains together

          def reverseChain(index: Int): Unit = {
            //        if (buildLog) buildLog.chainReverse(index)
            /* chains(index) =*/ chains(index).reverseInPlace() // gee, that's easy
          }

          def appendChain(index1: Int, index2: Int) = {
            // index1 gets index2 appended to it, and index2 is removed
            val chain1 = chains(index1)
            val chain2 = chains(index2)
            var tail = chain1.last
            val tail2 = chain1.secondLast
            val head = chain2.first
            val head2 = chain2.second

            if (EpsilonOps.pointsCollinear(tail2, tail, head)) {
              // tail isn't needed because it's directly between tail2 and head
              // tail2 ---tail---> head
              //          if (buildLog)
              //            buildLog.chainRemoveTail(index1, tail)
              chain1.removeLast()
              tail = tail2 // old tail is gone... new tail is what tail2 was
            }

            if (EpsilonOps.pointsCollinear(tail, head, head2)) {
              // head isn't needed because it's directly between tail and head2
              // tail ---head---> head2
              //          if (buildLog)            buildLog.chainRemoveHead(index2, head)
              chain2.removeFirst()
            }

            //        if (buildLog)          buildLog.chainJoin(index1, index2)
            chains(index1).concatWithUnsafe(chain2)
            chains.remove(index2)
          }

          val F = first_match.index
          val S = second_match.index

          //      if (buildLog)   buildLog.chainConnect(F, S)

          val reverseF = chains(F).length < chains(S).length // reverse the shorter chain, if needed
          if (first_match.matches_head) {
            if (second_match.matches_head) {
              if (reverseF) {
                // <<<< F <<<< --- >>>> S >>>>
                reverseChain(F)
                // >>>> F >>>> --- >>>> S >>>>
                appendChain(F, S)
              }
              else {
                // <<<< F <<<< --- >>>> S >>>>
                reverseChain(S)
                // <<<< F <<<< --- <<<< S <<<<   logically same as:
                // >>>> S >>>> --- >>>> F >>>>
                appendChain(S, F)
              }
            }
            else {
              // <<<< F <<<< --- <<<< S <<<<   logically same as:
              // >>>> S >>>> --- >>>> F >>>>
              appendChain(S, F)
            }
          }
          else {
            if (second_match.matches_head) {
              // >>>> F >>>> --- >>>> S >>>>
              appendChain(F, S)
            }
            else {
              if (reverseF) {
                // >>>> F >>>> --- <<<< S <<<<
                reverseChain(F)
                // <<<< F <<<< --- <<<< S <<<<   logically same as:
                // >>>> S >>>> --- >>>> F >>>>
                appendChain(S, F)
              }
              else {
                // >>>> F >>>> --- <<<< S <<<<
                reverseChain(S)
                // >>>> F >>>> --- >>>> S >>>>
                appendChain(F, S)
              }
            }
          }
        }
      }
    })

    return regions
  }

}
