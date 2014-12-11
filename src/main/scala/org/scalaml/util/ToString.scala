package org.scalaml.util

import org.scalaml.core.Types.ScalaMl._

object ToString {
	import java.text.DecimalFormat
	
	class Formatter(align: String, fmtStr: String) {
		val fmt = new DecimalFormat(fmtStr)
		
		def description(x: Double): String = s"${String.format(align, fmt.format(x))}"
		def description(n: Int): String = String.format(align, n.toString)
		def description(s: String): String =  String.format(align, s)
		def description[T](t: T): String = String.format(align, t.toString)
	}
	
	object ShortFormatter extends Formatter("%8s", "#,##0.000")
	object longFormatter extends Formatter("%16s", "#,##0.00000000")
	
	
	def toString(xy: XYTSeries, xLabel: String, yLabel: String, shortFormat: Boolean, labels: Array[String] = Array.empty): String = {
		require(xy != null && xy.size > 0, "ToString.toString XYTSeries is undefined")
		if(labels.size > 0)
			require(xy.size == labels.size, s"ToString.toString data size ${xy.size} is difference from number of labels ${labels.size}")
			
		val fmt = if(shortFormat) ShortFormatter else longFormatter
			
		val buf = new StringBuilder(s"$xLabel\t$yLabel\n")
		if(labels.size == 0)
			buf.append(xy.foldLeft(new StringBuilder)((buf, xy) => 
				buf.append(s"${fmt.description(xy._1)}${fmt.description(xy._2)}\n")).toString)
		else 
			buf.append(xy.zip(labels).foldLeft(new StringBuilder)((buf, xy) => 
				buf.append(s"${fmt.description(xy._2)}${fmt.description(xy._1._1)}${fmt.description(xy._1._2)}\n")).toString)
		buf.toString
	}

		
	def toString[T](x: Array[T], label: String, shortFormat: Boolean): String = {
		val fmt = if(shortFormat) ShortFormatter else longFormatter
						
		val buf = new StringBuilder
		if(label.size > 0)
			buf.append(s"${fmt.description(label)}\n")
		buf.append(x.zipWithIndex.foldLeft(new StringBuilder)((buf, x) => buf.append(s"${x._2}  ${fmt.description(x._1)}\n")).toString)
		buf.toString
	}
		
		
	def toString(x:DblVector): String = 
		x.zipWithIndex.foldLeft(new StringBuilder)((buf, x) => 
			buf.append(s"${x._2}  ${ShortFormatter.description(x._1)} ")).toString

	def toString(x: Double, label: String, shortFormat: Boolean): String = {
		val fmt = if(shortFormat) ShortFormatter else longFormatter
						
		val buf = new StringBuilder
		if(label.length > 1)
			buf.append(label)
		buf.append(s" ${fmt.description(x)}")
		buf.toString
	}
		
		
	def toString(m: DblMatrix, shortFormat: Boolean): String = {	
		val fmt = if(shortFormat) ShortFormatter else longFormatter
					
		val buf = new StringBuilder
		buf.append(Range(0, m(0).size).foldLeft(new StringBuilder)((buf, n) => buf.append(s"${fmt.description(n)}")).toString)
			
		Range(0, m.size).foreach(i => {
			buf.append(s"\n${fmt.description(i)}")
			buf.append(Range(0, m(0).size).foldLeft(new StringBuilder)((buf, j) => buf.append(s"${fmt.description(m(i)(j))}")).toString)
		})
		buf.toString
	}

}

// --------------------------  EOF --------------------------------