package domain



class Dragon(
     velocidadBase : Int = 60,
     danioBase: Int,
     pesoBase : Int,
     requisitos : List[RequisitoMontura] = List()
){
  def danio = danioBase
	def velocidad : Int = this.velocidadBase - this.pesoBase
	def capacidadDeCarga =  0.2 * pesoBase

	
	def cumpleRequisitoBasico(vikingo:Vikingo) = capacidadDeCarga >= vikingo.peso 
	def cumpleRequisitos(vikingo: Vikingo) = requisitos.forall(requisito => requisito.apply(vikingo, this))
  def puedeSerMontadoPor(vikingo : Vikingo) = cumpleRequisitoBasico(vikingo) && cumpleRequisitos(vikingo)

}

case class FuriaNocturna(danioBase:Int,requisitos:List[RequisitoMontura])
    extends Dragon(180,danioBase,500,vanidoso +: requisitos)

case class NadderMortifero(requisitos:List[RequisitoMontura])
  extends Dragon(60,15000,500,requisitos)

case class Gronckle(peso:Int,pesoMontura:Int,requisitos:List[RequisitoMontura])
  extends Dragon(30,5*peso,peso,pesado(pesoMontura) +: requisitos)