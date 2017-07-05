package domain

trait Item {
  
  def estadisticas(vikingo:Vikingo) : Vikingo
  
  def luegoDePosta(vikingo:Vikingo) : Vikingo
}

case class Arma(danioItem:Int = 0,velocidadItem:Int = 0,pesoItem: Int = 0) extends Item{
  
  def estadisticas(vikingo: Vikingo) = 
    vikingo.copy(vikingo.velocidadBase + velocidadItem,vikingo.barbarosidad + danioItem,vikingo.pesoBase + pesoItem)
    
  def luegoDePosta(vikingo: Vikingo) = vikingo
}

case class Consumible(porcentajeHambre : Int) extends Item{
  
  def estadisticas(vikingo: Vikingo) = vikingo
  
  def luegoDePosta(vikingo: Vikingo) = vikingo.disminuirHambre(porcentajeHambre)
}