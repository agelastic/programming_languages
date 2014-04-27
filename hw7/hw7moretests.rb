# University of Washington, Programming Languages, Homework 7,
# hw7testsprovided.rb

require "./hw7.rb"

#  Will not work completely until you implement all the classes and their methods

# Will print only if code has errors; prints nothing if all tests pass

# These tests do NOT cover all the various cases, especially for intersection

#Constants for testing
ZERO = 0.0
ONE = 1.0
TWO = 2.0
THREE = 3.0
FOUR = 4.0
FIVE = 5.0
SIX = 6.0
SEVEN = 7.0
EIGHT = 8.0
NINE = 9.0
TEN = 10.0


# adaptation from https://class.coursera.org/proglang-2012-001/forum/thread?thread_id=3479
class GeometryExpression
    def real_close(r1,r2)
        (r1 - r2).abs < GeometryExpression::Epsilon
    end
    def real_close_point(x1,y1,x2,y2)
        real_close(x1,x2) && real_close(y1,y2)
    end
end

class NoPoints < GeometryValue
  def ==(np)
    self.class == np.class
  end
end

class Point < GeometryValue
  def ==(p)
    self.class == p.class and real_close_point(@x, @y, p.x, p.y)
  end
end

class Line < GeometryValue
  def ==(l)
    self.class == l.class and real_close_point(@m, @b, l.m, l.b)
  end
end

class VerticalLine < GeometryValue
  def ==(vl)
    self.class == vl.class and real_close(@x, vl.x)
  end
end

class LineSegment < GeometryValue
  def ==(ls)
    self.class == ls.class and
    real_close_point(@x1,@y1,ls.x1,ls.y1) and
    real_close_point(@x2,@y2,ls.x2,ls.y2)
  end
end

class Intersect < GeometryExpression
  attr_reader :e1, :e2
  def ==(i)
    self.class == i.class and @e1 == i.e1 and @e2 == i.e2
  end
end

class Let < GeometryExpression
  attr_reader :s, :e1, :e2
  def ==(l)
    self.class == l.class and @s == l.s and @e1 == l.e1 and @e2 == l.e2
  end
end

class Var < GeometryExpression
  attr_reader :s
  def ==(v)
    self.class == v.class and @s == v.s
  end
end

class Shift < GeometryExpression
  attr_reader :dx, :dy, :e
  def ==(s)
    self.class == s.class and real_close_point(@dx, @dy, s.dx, s.dy) and @e == s.e
  end
end

def equal(e1, e2)
    e1 == e2
end

tests = []
tests[0] = equal(NoPoints.new.preprocess_prog, NoPoints.new)
tests[1] = equal(Point.new(ONE, ONE).preprocess_prog, Point.new(ONE, ONE))
tests[2] = equal(Line.new(ONE, TWO).preprocess_prog, Line.new(ONE, TWO))
tests[3] = equal(VerticalLine.new(TWO).preprocess_prog, VerticalLine.new(TWO))
tests[4] = equal(LineSegment.new(ONE, ONE, ONE, ONE).preprocess_prog, Point.new(ONE, ONE))
tests[5] = equal(LineSegment.new(ONE, ONE, ONE, TWO).preprocess_prog, LineSegment.new(ONE, ONE, ONE, TWO))
tests[6] = equal(LineSegment.new(ONE, ONE, ONE, ZERO).preprocess_prog, LineSegment.new(ONE, ZERO, ONE, ONE))
tests[7] = equal(LineSegment.new(ONE, ONE, TWO, ONE).preprocess_prog, LineSegment.new(ONE, ONE, TWO, ONE))
tests[8] = equal(LineSegment.new(ONE, ZERO, TWO, ONE).preprocess_prog, LineSegment.new(ONE, ZERO, TWO, ONE))
tests[9] = equal(LineSegment.new(ONE, ONE, TWO, ZERO).preprocess_prog, LineSegment.new(ONE, ONE, TWO, ZERO))
tests[10] = equal(LineSegment.new(TWO, ONE, ONE, ONE).preprocess_prog, LineSegment.new(ONE, ONE, TWO, ONE))
tests[11] = equal(LineSegment.new(TWO, ZERO, ONE, ONE).preprocess_prog, LineSegment.new(ONE, ONE, TWO, ZERO))
tests[12] = equal(LineSegment.new(TWO, ONE, ONE, ZERO).preprocess_prog, LineSegment.new(ONE, ZERO, TWO, ONE))
tests[13] = equal(LineSegment.new(1.00000999, ONE, ONE, TWO).preprocess_prog, LineSegment.new(1.00000999, ONE, ONE, TWO))
tests[14] = equal(Let.new("x", LineSegment.new(ONE, ONE, ONE, ONE), Var.new("x")).preprocess_prog, Let.new("x", Point.new(ONE, ONE), Var.new("x")))
tests[15] = equal(Shift.new(ONE, ONE, LineSegment.new(ONE, ONE, ONE, ONE)).preprocess_prog, Shift.new(ONE, ONE, Point.new(ONE, ONE)))
tests[16] = equal(Intersect.new(LineSegment.new(ONE, ONE, ONE, ONE), LineSegment.new(TWO, ONE, ONE, ZERO)).preprocess_prog, Intersect.new(Point.new(ONE, ONE), LineSegment.new(ONE, ZERO, TWO, ONE)))
tests[17] = equal(Shift.new(ONE, ONE, Intersect.new(LineSegment.new(ONE, ONE, ONE, ONE), LineSegment.new(TWO, ONE, ONE, ZERO))).preprocess_prog, Shift.new(ONE, ONE, Intersect.new(Point.new(ONE, ONE), LineSegment.new(ONE, ZERO, TWO, ONE))))
tests[81] = Shift.new(ONE, TWO, LineSegment.new(3.2,4.1,3.2,4.1)).preprocess_prog == Shift.new(ONE, TWO, Point.new(3.2, 4.1))
tests[80] = Let.new("a", LineSegment.new(3.2,4.1,3.2,4.1), LineSegment.new(3.2,4.1,3.2,4.1)).preprocess_prog == Let.new("a", Point.new(3.2, 4.1), Point.new(3.2, 4.1))
tests[82] = Intersect.new(LineSegment.new(3.2,4.1,3.2,4.1), LineSegment.new(3.2,4.1,3.2,4.1)).preprocess_prog == Intersect.new(Point.new(3.2, 4.1), Point.new(3.2, 4.1))

# eval_prog
# Shift
tests[20] = equal(Shift.new(ONE, ONE, Point.new(TWO, THREE)).eval_prog([]), Point.new(THREE, FOUR))
tests[21] = equal(Shift.new(TWO, 10.3, Line.new(THREE, FIVE)).eval_prog([]), Line.new(THREE, 9.3))
tests[22] = equal(Shift.new(ONE, TWO, VerticalLine.new(FIVE)).eval_prog([]), VerticalLine.new(6.0))
tests[23] = equal(Shift.new(TWO, TWO, LineSegment.new(ONE, ONE, ONE, ONE)).preprocess_prog.eval_prog([]), Point.new(THREE, THREE))
tests[24] = equal(Shift.new(ONE, TWO, LineSegment.new(ONE, ONE, THREE, THREE)).preprocess_prog.eval_prog([]), LineSegment.new(TWO, THREE, FOUR, FIVE))
tests[25] = equal(Shift.new(ONE, ONE, Shift.new(ONE, ONE, Shift.new(ONE, ONE, Point.new(ONE, ONE)))).preprocess_prog.eval_prog([]), Point.new(FOUR, FOUR))
tests[83] = Shift.new(THREE, FOUR, Point.new(FOUR, FOUR)).eval_prog([]) == Point.new(SEVEN, EIGHT)
tests[84] = Shift.new(THREE, FOUR, Line.new(FOUR, FOUR)).eval_prog([]) == Line.new(FOUR, -FOUR)
tests[85] = Shift.new(THREE, FOUR, VerticalLine.new(FOUR)).eval_prog([]) == VerticalLine.new(SEVEN)
tests[86] = Shift.new(THREE, FOUR, LineSegment.new(FOUR, THREE, 12.0, -TWO)).eval_prog([]) == LineSegment.new(SEVEN, SEVEN, 15.0, TWO)

# Let
tests[31] = equal(Let.new("x", Point.new(TWO, THREE), Var.new("x")).preprocess_prog.eval_prog([]), Point.new(TWO, THREE))
tests[32] = equal(Let.new("x", Line.new(THREE, FIVE), Var.new("x")).preprocess_prog.eval_prog([]), Line.new(THREE, FIVE))
tests[33] = equal(Let.new("x", VerticalLine.new(ONE), Var.new("x")).preprocess_prog.eval_prog([]), VerticalLine.new(ONE))
tests[34] = equal(Let.new("x", LineSegment.new(TWO, TWO, ONE, ONE), Var.new("x")).preprocess_prog.eval_prog([]), LineSegment.new(ONE, ONE, TWO, TWO))
tests[35] = equal(Let.new("x", Point.new(ONE, ONE), Let.new("x", Point.new(TWO, TWO), Var.new("x"))).preprocess_prog.eval_prog([]), Point.new(TWO, TWO))

# Let + Shift
tests[40] = equal(Let.new("x", LineSegment.new(ONE, ONE, ONE, ONE), Shift.new(ONE, ONE, Var.new("x"))).preprocess_prog.eval_prog([]), Point.new(TWO, TWO))
tests[41] = equal(Shift.new(ONE, ONE, Let.new("x", VerticalLine.new(TWO), Var.new("x"))).preprocess_prog.eval_prog([]), VerticalLine.new(THREE))

# Intersect
# NoPoints
tests[45] = equal(Intersect.new(NoPoints.new, NoPoints.new).preprocess_prog.eval_prog([]), NoPoints.new)
tests[46] = equal(Intersect.new(NoPoints.new, Point.new(ONE, TWO)).preprocess_prog.eval_prog([]), NoPoints.new)
tests[47] = equal(Intersect.new(NoPoints.new, Line.new(ONE, ONE)).preprocess_prog.eval_prog([]), NoPoints.new)
tests[48] = equal(Intersect.new(NoPoints.new, VerticalLine.new(ONE)).preprocess_prog.eval_prog([]), NoPoints.new)
tests[49] = equal(Intersect.new(NoPoints.new, LineSegment.new(ONE, ONE, THREE, THREE)).preprocess_prog.eval_prog([]), NoPoints.new)
tests[50] = equal(Intersect.new(Point.new(ONE, ONE), Point.new(1.00000999, ONE)).preprocess_prog.eval_prog([]), Point.new(1.00000999, ONE))
tests[51] = equal(Intersect.new(Point.new(ONE, ONE), Point.new(1.0999, ONE)).preprocess_prog.eval_prog([]), NoPoints.new)

# Point
tests[52] = equal(Intersect.new(Point.new(ONE, FOUR), Line.new(ONE, THREE)).preprocess_prog.eval_prog([]), Point.new(ONE, FOUR))
tests[53] = equal(Intersect.new(Point.new(TWO, FOUR), Line.new(ONE, THREE)).preprocess_prog.eval_prog([]), NoPoints.new)
tests[54] = equal(Intersect.new(Point.new(TWO, FOUR), VerticalLine.new(TWO)).preprocess_prog.eval_prog([]), Point.new(TWO, FOUR))
tests[55] = equal(Intersect.new(Point.new(FOUR, FOUR), VerticalLine.new(TWO)).preprocess_prog.eval_prog([]), NoPoints.new)
tests[56] = equal(Intersect.new(Point.new(TWO, TWO), LineSegment.new(ZERO, ZERO, THREE, THREE)).preprocess_prog.eval_prog([]), Point.new(TWO, TWO))
tests[57] = equal(Intersect.new(Point.new(FOUR, FOUR), LineSegment.new(ZERO, ZERO, THREE, THREE)).preprocess_prog.eval_prog([]), NoPoints.new)
tests[87] = Intersect.new(Point.new(FOUR, FOUR), Point.new(FOUR, FOUR)).eval_prog([]) == Point.new(FOUR, FOUR)
tests[88] = Intersect.new(Point.new(FOUR, FOUR), Point.new(FOUR, 4.1)).eval_prog([]).class == NoPoints.new.class
tests[89] = Intersect.new(Point.new(FOUR, FOUR), Line.new(FOUR, 4.1)).eval_prog([]).class == NoPoints.new.class
tests[90] = Intersect.new(Point.new(ONE, EIGHT), Line.new(FOUR, FOUR)).eval_prog([]) == Point.new(ONE, EIGHT)
tests[91] = Intersect.new(Point.new(FIVE, FOUR), VerticalLine.new(FOUR)).eval_prog([]).class == NoPoints.new.class
tests[92] = Intersect.new(Point.new(FOUR, FOUR), VerticalLine.new(FOUR)).eval_prog([]) == Point.new(FOUR, FOUR)
tests[93] = Intersect.new(Point.new(TWO, TWO), LineSegment.new(ONE, ONE, FOUR, FOUR)).eval_prog([]) == Point.new(TWO, TWO)
tests[94] = Intersect.new(Point.new(4.1, 4.1), LineSegment.new(ONE, ONE, FOUR, FOUR)).eval_prog([]).class == NoPoints.new.class

# Line
tests[58] = equal(Intersect.new(Line.new(ONE, TWO), Line.new(TWO, ZERO)).preprocess_prog.eval_prog([]), Point.new(TWO, FOUR))
tests[59] = equal(Intersect.new(Line.new(ONE, TWO), Line.new(ONE, THREE)).preprocess_prog.eval_prog([]), NoPoints.new)
tests[60] = equal(Intersect.new(Line.new(ONE, TWO), VerticalLine.new(TWO)).preprocess_prog.eval_prog([]), Point.new(TWO, FOUR))
tests[61] = equal(Intersect.new(Line.new(ONE, ZERO), LineSegment.new(ZERO, ZERO, FOUR, FOUR)).preprocess_prog.eval_prog([]), LineSegment.new(ZERO, ZERO, FOUR, FOUR))
tests[62] = equal(Intersect.new(Line.new(TWO, ZERO), LineSegment.new(ZERO, ZERO, FOUR, FOUR)).preprocess_prog.eval_prog([]), Point.new(ZERO, ZERO))
tests[63] = equal(Intersect.new(Line.new(TWO, ZERO), LineSegment.new(ONE, ONE, FOUR, FOUR)).preprocess_prog.eval_prog([]), NoPoints.new)
tests[95] = Intersect.new(Line.new(FOUR, FOUR), Point.new(ONE, EIGHT)).eval_prog([]) == Point.new(ONE, EIGHT)
tests[96] = Intersect.new(Line.new(FOUR, FOUR), Point.new(FOUR, 4.1)).eval_prog([]).class == NoPoints.new.class
tests[97] = Intersect.new(Line.new(FOUR, FOUR), Line.new(FOUR, 4.1)).eval_prog([]).class == NoPoints.new.class
tests[98] = Intersect.new(Line.new(ONE, SEVEN), Line.new(FOUR, FOUR)).eval_prog([]) == Point.new(ONE, EIGHT)
tests[99] = Intersect.new(Line.new(FOUR, FOUR), VerticalLine.new(FOUR)).eval_prog([]) == Point.new(FOUR, 20.0)
tests[100] = Intersect.new(Line.new(-ONE, ONE), LineSegment.new(ONE, ONE, FOUR, FOUR)).eval_prog([]).class == NoPoints.new.class
tests[101] = Intersect.new(Line.new(-ONE, TWO), LineSegment.new(ONE, ONE, FOUR, FOUR)).eval_prog([]) == Point.new(ONE, ONE)

# VerticalLine
tests[64] = equal(Intersect.new(VerticalLine.new(ONE), VerticalLine.new(TWO)).preprocess_prog.eval_prog([]), NoPoints.new)
tests[65] = equal(Intersect.new(VerticalLine.new(ONE), LineSegment.new(ONE, ZERO, ONE, FOUR)).preprocess_prog.eval_prog([]), LineSegment.new(ONE, ZERO, ONE, FOUR))
tests[66] = equal(Intersect.new(VerticalLine.new(ONE), LineSegment.new(ZERO, ONE, THREE, ONE)).preprocess_prog.eval_prog([]), Point.new(ONE, ONE))
tests[67] = equal(Intersect.new(VerticalLine.new(ONE), LineSegment.new(TWO, TWO, FOUR, FOUR)).preprocess_prog.eval_prog([]), NoPoints.new)
tests[102] = Intersect.new(VerticalLine.new(FOUR), Point.new(FOUR, EIGHT)).eval_prog([]) == Point.new(FOUR, EIGHT)
tests[103] = Intersect.new(VerticalLine.new(FOUR), Point.new(4.1, FOUR)).eval_prog([]).class == NoPoints.new.class
tests[104] = Intersect.new(VerticalLine.new(FOUR), Line.new(FOUR, FOUR)).eval_prog([]) == Point.new(FOUR, 20.0)
tests[105] = Intersect.new(VerticalLine.new(FOUR), VerticalLine.new(4.1)).eval_prog([]).class == NoPoints.new.class
tests[106] = Intersect.new(VerticalLine.new(FOUR), VerticalLine.new(FOUR)).eval_prog([]) == VerticalLine.new(FOUR)
tests[107] = Intersect.new(VerticalLine.new(4.1), LineSegment.new(ONE, ONE, FOUR, FOUR)).eval_prog([]).class == NoPoints.new.class
tests[108] = Intersect.new(VerticalLine.new(TWO), LineSegment.new(ONE, ONE, FOUR, FOUR)).eval_prog([]) == Point.new(TWO, TWO)

# LineSegment
tests[70] = equal(Intersect.new(LineSegment.new(ZERO, ZERO, ZERO, FOUR), LineSegment.new(ONE, ZERO, ONE, FOUR)).preprocess_prog.eval_prog([]), NoPoints.new)
tests[71] = equal(Intersect.new(LineSegment.new(ZERO, ZERO, FOUR, ZERO), LineSegment.new(ZERO, ONE, FOUR, ONE)).preprocess_prog.eval_prog([]), NoPoints.new)
tests[72] = equal(Intersect.new(LineSegment.new(TWO, ZERO, TWO, FOUR), LineSegment.new(ZERO, TWO, FOUR, TWO)).preprocess_prog.eval_prog([]), Point.new(TWO, TWO))
tests[73] = equal(Intersect.new(LineSegment.new(ZERO, ZERO, ZERO, FOUR), LineSegment.new(ZERO, ONE, ZERO, THREE)).preprocess_prog.eval_prog([]), LineSegment.new(ZERO, ONE, ZERO, THREE))
tests[74] = equal(Intersect.new(LineSegment.new(ZERO, ONE, ZERO, THREE), LineSegment.new(ZERO, ZERO, ZERO, FOUR)).preprocess_prog.eval_prog([]), LineSegment.new(ZERO, ONE, ZERO, THREE))
tests[75] = equal(Intersect.new(LineSegment.new(ZERO, ONE, ZERO, THREE), LineSegment.new(ZERO, ONE, ZERO, THREE)).preprocess_prog.eval_prog([]), LineSegment.new(ZERO, ONE, ZERO, THREE))
tests[76] = equal(Intersect.new(LineSegment.new(ZERO, ZERO, ZERO, THREE), LineSegment.new(ZERO, ONE, ZERO, FOUR)).preprocess_prog.eval_prog([]), LineSegment.new(ZERO, ONE, ZERO, THREE))

# Intersection tests with LineSegment and Point/Line/VerticalLine
tests[109] = Intersect.new(LineSegment.new(ONE, ONE, FOUR, FOUR), Point.new(TWO, TWO)).eval_prog([]) == Point.new(TWO, TWO)
tests[110] = Intersect.new(LineSegment.new(ONE, ONE, FOUR, FOUR), Point.new(4.1, 4.1)).eval_prog([]).class == NoPoints.new.class
tests[111] = Intersect.new(LineSegment.new(ONE, ONE, FOUR, FOUR), Line.new(-ONE, ONE)).eval_prog([]).class == NoPoints.new.class
tests[112] = Intersect.new(LineSegment.new(ONE, ONE, FOUR, FOUR), Line.new(-ONE, TWO)).eval_prog([]) == Point.new(ONE, ONE)
tests[113] = Intersect.new(LineSegment.new(ONE, ONE, FOUR, FOUR), VerticalLine.new(4.1)).eval_prog([]).class == NoPoints.new.class
tests[114] = Intersect.new(LineSegment.new(ONE, ONE, FOUR, FOUR), VerticalLine.new(TWO)).eval_prog([]) == Point.new(TWO, TWO)

# Intersection between a vertical LineSegment and Point/Line/VerticalLine
tests[115] = Intersect.new(LineSegment.new(ONE, ONE, ONE, FOUR), Point.new(ONE, TWO)).eval_prog([]) == Point.new(ONE, TWO)
tests[116] = Intersect.new(LineSegment.new(ONE, ONE, ONE, FOUR), Point.new(ONE, 4.1)).eval_prog([]).class == NoPoints.new.class
tests[117] = Intersect.new(LineSegment.new(ONE, ONE, ONE, FOUR), Line.new(-ONE, ONE)).eval_prog([]).class == NoPoints.new.class
tests[118] = Intersect.new(LineSegment.new(ONE, ONE, ONE, FOUR), Line.new(-ONE, FOUR)).eval_prog([]) == Point.new(ONE, THREE)
tests[119] = Intersect.new(LineSegment.new(ONE, ONE, ONE, FOUR), VerticalLine.new(4.1)).eval_prog([]).class == NoPoints.new.class
tests[120] = Intersect.new(LineSegment.new(ONE, ONE, ONE, FOUR), VerticalLine.new(ONE)).eval_prog([]) == LineSegment.new(ONE, ONE, ONE, FOUR)

# intersection between two oblique LineSegments
tests[121] = Intersect.new(LineSegment.new(ONE, ONE, FOUR, FOUR), LineSegment.new(4.1, 4.1, FIVE, FIVE)).eval_prog([]).class == NoPoints.new.class
tests[122] = Intersect.new(LineSegment.new(ONE, ONE, FOUR, FOUR), LineSegment.new(TWO, TWO, THREE, THREE)).eval_prog([]) == LineSegment.new(TWO, TWO, THREE, THREE)
tests[123] = Intersect.new(LineSegment.new(ONE, ONE, FOUR, FOUR), LineSegment.new(-ONE, -ONE, THREE, THREE)).eval_prog([]) == LineSegment.new(ONE, ONE, THREE, THREE)
tests[124] = Intersect.new(LineSegment.new(ONE, ONE, FOUR, FOUR), LineSegment.new(TWO, TWO, FIVE, FIVE)).eval_prog([]) == LineSegment.new(TWO, TWO, FOUR, FOUR)
tests[125] = Intersect.new(LineSegment.new(ONE, ONE, FOUR, FOUR), LineSegment.new(FOUR, FOUR, FIVE, FIVE)).eval_prog([]) == Point.new(FOUR, FOUR)
tests[126] = Intersect.new(LineSegment.new(ONE, ONE, FOUR, FOUR), LineSegment.new(-FOUR, -FOUR, ONE, ONE)).eval_prog([]) == Point.new(ONE, ONE)
tests[127] = Intersect.new(LineSegment.new(TWO, TWO, THREE, THREE), LineSegment.new(ONE, ONE, FIVE, FIVE)).eval_prog([]) == LineSegment.new(TWO, TWO, THREE, THREE)

# intersection between two vertical LineSegments
tests[128] = Intersect.new(LineSegment.new(ONE, ONE, ONE, FOUR), LineSegment.new(ONE, 4.1, ONE, FIVE)).eval_prog([]).class == NoPoints.new.class
tests[129] = Intersect.new(LineSegment.new(ONE, ONE, ONE, FOUR), LineSegment.new(ONE, TWO, ONE, THREE)).eval_prog([]) == LineSegment.new(ONE, TWO, ONE, THREE)
tests[130] = Intersect.new(LineSegment.new(ONE, ONE, ONE, FOUR), LineSegment.new(ONE, -ONE, ONE, THREE)).eval_prog([]) == LineSegment.new(ONE, ONE, ONE, THREE)
tests[131] = Intersect.new(LineSegment.new(ONE, ONE, ONE, FOUR), LineSegment.new(ONE, TWO, ONE, FIVE)).eval_prog([]) == LineSegment.new(ONE, TWO, ONE, FOUR)
tests[132] = Intersect.new(LineSegment.new(ONE, ONE, ONE, FOUR), LineSegment.new(ONE, FOUR, ONE, FIVE)).eval_prog([]) == Point.new(ONE, FOUR)
tests[133] = Intersect.new(LineSegment.new(ONE, ONE, ONE, FOUR), LineSegment.new(ONE, -FOUR, ONE, ONE)).eval_prog([]) == Point.new(ONE, ONE)
tests[134] = Intersect.new(LineSegment.new(ONE, TWO, ONE, THREE), LineSegment.new(ONE, ONE, ONE, FIVE)).eval_prog([]) == LineSegment.new(ONE, TWO, ONE, THREE)
tests[135] = Intersect.new(Point.new(2.5,1.5),Intersect.new(LineSegment.new(2.0,1.0,3.0,2.0),Intersect.new(LineSegment.new(0.0,0.0,2.5,1.5),Line.new(1.0,-1.0)))).eval_prog([]) == Point.new(2.5,1.5)
tests[136] = Point.new(5.0,7.0).intersect(LineSegment.new(-1.0,2.0,5.0,7.0)) == Point.new(5.0,7.0)
tests[137] = Point.new(5.0,7.0).intersectLineSegment(LineSegment.new(-1.0,2.0,5.0,7.0)) == Point.new(5.0,7.0)
tests[138] = LineSegment.new(-1.0,2.0,5.0,7.0).intersect(Point.new(5.0,7.0)) == Point.new(5.0,7.0)
tests[139] = LineSegment.new(-1.0,2.0,5.0,7.0).intersectPoint(Point.new(5.0,7.0)) == Point.new(5.0,7.0)
tests[140] = Line.new(5.0,2.0).intersect(Line.new(7.0,-1.0)) == Point.new(1.5,9.5)
tests[141] = Line.new(5.0,2.0).intersectLine(Line.new(7.0,-1.0)) == Point.new(1.5,9.5)
tests[142] = Line.new(5.0,0.0).intersect(LineSegment.new(1.0,5.0,2.0,2.0)) == Point.new(1.0,5.0)
tests[143] = Line.new(5.0,0.0).intersect(LineSegment.new(-1.0,-1.0,1.0,5.0)) == Point.new(1.0,5.0)
tests[144] = Line.new(5.0,0.0).intersect(LineSegment.new(-1.0,-1.0,1.0,5.0)) == Point.new(1.0,5.0)
tests[145] = Line.new(5.0,0.0).intersectLineSegment(LineSegment.new(1.0,5.0,2.0,2.0)) == Point.new(1.0,5.0)
tests[146] = Line.new(5.0,0.0).intersectLineSegment(LineSegment.new(-1.0,-1.0,1.0,5.0)) == Point.new(1.0,5.0)
tests[147] = Line.new(5.0,0.0).intersectLineSegment(LineSegment.new(-1.0,-1.0,1.0,5.0)) == Point.new(1.0,5.0)
tests[148] = LineSegment.new(1.0,5.0,2.0,2.0).intersect(Line.new(5.0,0.0)) == Point.new(1.0,5.0)
tests[149] = LineSegment.new(-1.0,-1.0,1.0,5.0).intersect(Line.new(5.0,0.0)) == Point.new(1.0,5.0)
tests[150] = LineSegment.new(-1.0,-1.0,1.0,5.0).intersect(Line.new(5.0,0.0)) == Point.new(1.0,5.0)
tests[151] = LineSegment.new(1.0,5.0,2.0,2.0).intersectLine(Line.new(5.0,0.0)) == Point.new(1.0,5.0)
tests[152] = LineSegment.new(-1.0,-1.0,1.0,5.0).intersectLine(Line.new(5.0,0.0)) == Point.new(1.0,5.0)
tests[153] = LineSegment.new(-1.0,-1.0,1.0,5.0).intersectLine(Line.new(5.0,0.0)) == Point.new(1.0,5.0)
tests[154] = VerticalLine.new(3.0).intersect(LineSegment.new(-1.0,-1.0,3.0,3.0)) == Point.new(3.0,3.0)
tests[155] = VerticalLine.new(3.0).intersectLineSegment(LineSegment.new(-1.0,-1.0,3.0,3.0)) == Point.new(3.0,3.0)
tests[156] = LineSegment.new(-1.0,-1.0,3.0,3.0).intersect(VerticalLine.new(3.0)) == Point.new(3.0,3.0)
tests[157] = LineSegment.new(-1.0,-1.0,3.0,3.0).intersectVerticalLine(VerticalLine.new(3.0)) == Point.new(3.0,3.0)
tests[158] = LineSegment.new(5.0,7.0,9.0,9.0).intersect(LineSegment.new(5.0,7.0,6.0,-1.0)) == Point.new(5.0,7.0)
tests[159] = LineSegment.new(5.0,2.0,9.0,9.0).intersect(LineSegment.new(-2.0,-1.0,5.0,2.0)) == Point.new(5.0,2.0)
tests[160] = LineSegment.new(2.0,3.0,8.0,4.0).intersect(LineSegment.new(1.0,1.0,8.0,4.0)) == Point.new(8.0,4.0)
tests[161] = LineSegment.new(5.0,7.0,9.0,9.0).intersectLineSegment(LineSegment.new(5.0,7.0,6.0,-1.0))  == Point.new(5.0,7.0)
tests[162] = LineSegment.new(5.0,2.0,9.0,9.0).intersectLineSegment(LineSegment.new(-2.0,-1.0,5.0,2.0)) == Point.new(5.0,2.0)
tests[163] = LineSegment.new(2.0,3.0,8.0,4.0).intersectLineSegment(LineSegment.new(1.0,1.0,8.0,4.0)) == Point.new(8.0,4.0)
tests.each_with_index {|v,i| puts "#{i}: #{v}"}