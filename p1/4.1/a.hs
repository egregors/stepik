-- Определите экземпляр класса Show для типа Color, сопоставляющий каждому из трех цветов его текстовое представление.

data Color = Red | Green | Blue

instance Show Color where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"
