package detector.gen.mutante.model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.google.gson.GsonBuilder;

public class Request implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1210530614460404556L;
	
	@JsonProperty("dna")
	private String[] dna;
	
	@JsonProperty("esMutante")
	private String esMutante;

	public String[] getDna() {
		return dna;
	}

	public void setDna(String[] dna) {
		this.dna = dna;
	}
	
	public String getEsMutante() {
		return esMutante;
	}

	public void setEsMutante(String esMutante) {
		this.esMutante = esMutante;
	}

	@Override
	public String toString() {
		return new GsonBuilder().create().toJson(this);
	}
}
