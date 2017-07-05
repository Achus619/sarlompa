package domain 

case class Torneo(
		  postas:   List[Posta],
		  participantes: List[ParticipanteTorneo],
		  dragones: List[Dragon],
		  reglas : Reglas
		)
{
  
  def competir : Option[ParticipanteTorneo] = {
    reglas.decidirGanador(jugarPostas)
  }
    
  def prepararParticipantes = { participantes map {
    case vikingo:Vikingo => List() :+ vikingo
    case equipo:Equipo => equipo.vikingos

    }
    
  }
  
  def jugarPostas : List[Vikingo] = {
    postas.foldLeft(prepararParticipantes.flatten){(participantesEnJuego,posta) =>
      if(hayMasDeUnVikingo(participantesEnJuego) )
        jugarPosta(participantesEnJuego, posta)
      else
        participantesEnJuego
    }
  }
  
  def hayMasDeUnVikingo(participantes: List[ParticipanteTorneo]) = participantes.size > 1
  
  def jugarPosta(vikingosEnJuego: List[Vikingo],posta:Posta) : List[Vikingo] = {
    val participantesListos = reglas.eleccionDeDragones(vikingosEnJuego,posta,dragones)
    val ganadores = posta.participar(participantesListos)
    val vikingosGanadores = ganadores.map(_.vikingo)
    reglas.quienesAvanzan(vikingosGanadores)
    
  }
}





