Scala for Machine Learning
========================
Directory for output from execution of test cases
The class DataSink is responsible to write content into this directory using the |> operator

Declaration
class DataSink[T <% String](sinkName: String) {
  def write(content: String): Boolean
  override def |> : PartialFunction[List[XTSeries[T]], Int]
}

Usage:
DataSink[Double](output) |> input.foldLeft(List[XTSeries[Double]]())((sk, v) => v :: sk)