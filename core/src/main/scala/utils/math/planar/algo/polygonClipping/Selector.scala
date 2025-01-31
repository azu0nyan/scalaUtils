package utils.math.planar.algo.polygonClipping

object Selector {
  def select(segments: Iterable[Segment], selection: Array[Int]):Iterable[Segment] =
    segments
      .map(seg =>
      (seg,
        (if (seg.myFill.above.getOrElse(false)) 8 else 0) +
          (if (seg.myFill.below.getOrElse(false)) 4 else 0) +
          (if (seg.otherFill != null && seg.otherFill.above.getOrElse(false)) 2 else 0) +
          (if (seg.otherFill != null && seg.otherFill.below.getOrElse(false)) 1 else 0)
      ))
      .filter(si=> selection(si._2) != 0)
      .map(si => new Segment(
        si._1.start,
        si._1.end,
        new Fill(
          Some(selection(si._2) == 1),
          Some(selection(si._2) == 2),
        ),
        null
      ))


   def union(segments:Iterable[Segment]):Iterable[Segment] = { // primary | secondary
    // above1 below1 above2 below2    Keep?               Value
    //    0      0      0      0   =>   no                  0
    //    0      0      0      1   =>   yes filled below    2
    //    0      0      1      0   =>   yes filled above    1
    //    0      0      1      1   =>   no                  0
    //    0      1      0      0   =>   yes filled below    2
    //    0      1      0      1   =>   yes filled below    2
    //    0      1      1      0   =>   no                  0
    //    0      1      1      1   =>   no                  0
    //    1      0      0      0   =>   yes filled above    1
    //    1      0      0      1   =>   no                  0
    //    1      0      1      0   =>   yes filled above    1
    //    1      0      1      1   =>   no                  0
    //    1      1      0      0   =>   no                  0
    //    1      1      0      1   =>   no                  0
    //    1      1      1      0   =>   no                  0
    //    1      1      1      1   =>   no                  0
     select(segments, Array(
      0, 2, 1, 0,
      2, 2, 0, 0,
      1, 0, 1, 0,
      0, 0, 0, 0)
    )
  }
   def  intersect(segments:Iterable[Segment]):Iterable[Segment] = { // primary & secondary
    // above1 below1 above2 below2    Keep?               Value
    //    0      0      0      0   =>   no                  0
    //    0      0      0      1   =>   no                  0
    //    0      0      1      0   =>   no                  0
    //    0      0      1      1   =>   no                  0
    //    0      1      0      0   =>   no                  0
    //    0      1      0      1   =>   yes filled below    2
    //    0      1      1      0   =>   no                  0
    //    0      1      1      1   =>   yes filled below    2
    //    1      0      0      0   =>   no                  0
    //    1      0      0      1   =>   no                  0
    //    1      0      1      0   =>   yes filled above    1
    //    1      0      1      1   =>   yes filled above    1
    //    1      1      0      0   =>   no                  0
    //    1      1      0      1   =>   yes filled below    2
    //    1      1      1      0   =>   yes filled above    1
    //    1      1      1      1   =>   no                  0
    return select(segments, Array(
      0, 0, 0, 0,
      0, 2, 0, 2,
      0, 0, 1, 1,
      0, 2, 1, 0
    ))
  }

  def   difference(segments:Iterable[Segment]):Iterable[Segment] = { // primary - secondary
    // above1 below1 above2 below2    Keep?               Value
    //    0      0      0      0   =>   no                  0
    //    0      0      0      1   =>   no                  0
    //    0      0      1      0   =>   no                  0
    //    0      0      1      1   =>   no                  0
    //    0      1      0      0   =>   yes filled below    2
    //    0      1      0      1   =>   no                  0
    //    0      1      1      0   =>   yes filled below    2
    //    0      1      1      1   =>   no                  0
    //    1      0      0      0   =>   yes filled above    1
    //    1      0      0      1   =>   yes filled above    1
    //    1      0      1      0   =>   no                  0
    //    1      0      1      1   =>   no                  0
    //    1      1      0      0   =>   no                  0
    //    1      1      0      1   =>   yes filled above    1
    //    1      1      1      0   =>   yes filled below    2
    //    1      1      1      1   =>   no                  0
    return select(segments, Array(
      0, 0, 0, 0,
      2, 0, 2, 0,
      1, 1, 0, 0,
      0, 1, 2, 0
    ))
  }
    def differenceRev(segments:Iterable[Segment]):Iterable[Segment] = { // secondary - primary
    // above1 below1 above2 below2    Keep?               Value
    //    0      0      0      0   =>   no                  0
    //    0      0      0      1   =>   yes filled below    2
    //    0      0      1      0   =>   yes filled above    1
    //    0      0      1      1   =>   no                  0
    //    0      1      0      0   =>   no                  0
    //    0      1      0      1   =>   no                  0
    //    0      1      1      0   =>   yes filled above    1
    //    0      1      1      1   =>   yes filled above    1
    //    1      0      0      0   =>   no                  0
    //    1      0      0      1   =>   yes filled below    2
    //    1      0      1      0   =>   no                  0
    //    1      0      1      1   =>   yes filled below    2
    //    1      1      0      0   =>   no                  0
    //    1      1      0      1   =>   no                  0
    //    1      1      1      0   =>   no                  0
    //    1      1      1      1   =>   no                  0
    return select(segments, Array(
      0, 2, 1, 0,
      0, 0, 1, 1,
      0, 2, 0, 2,
      0, 0, 0, 0
    ));
  }
   def  xor(segments:Iterable[Segment]):Iterable[Segment] = { // primary ^ secondary
    // above1 below1 above2 below2    Keep?               Value
    //    0      0      0      0   =>   no                  0
    //    0      0      0      1   =>   yes filled below    2
    //    0      0      1      0   =>   yes filled above    1
    //    0      0      1      1   =>   no                  0
    //    0      1      0      0   =>   yes filled below    2
    //    0      1      0      1   =>   no                  0
    //    0      1      1      0   =>   no                  0
    //    0      1      1      1   =>   yes filled above    1
    //    1      0      0      0   =>   yes filled above    1
    //    1      0      0      1   =>   no                  0
    //    1      0      1      0   =>   no                  0
    //    1      0      1      1   =>   yes filled below    2
    //    1      1      0      0   =>   no                  0
    //    1      1      0      1   =>   yes filled above    1
    //    1      1      1      0   =>   yes filled below    2
    //    1      1      1      1   =>   no                  0
    return select(segments, Array(
      0, 2, 1, 0,
      2, 0, 0, 1,
      1, 0, 0, 2,
      0, 1, 2, 0
    ))
  }



}
