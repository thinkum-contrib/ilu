import Tutorial, Tutorial__skel

class Calculator (Tutorial__skel.Calculator):

	def __init__ (self):
		self.the_value = 0.0

	def SetValue (self, v):
		self.the_value = v

	def GetValue (self):
		return self.the_value

	def Add (self, v):
		self.the_value = self.the_value + v

	def Subtract (self, v):
		self.the_value = self.the_value - v

	def Multiply (self, v):
		self.the_value = self.the_value * v

	def Divide (self, v):
		try:
			self.the_value = self.the_value / v
		except ZeroDivisionError:
			raise Tutorial.DivideByZero
