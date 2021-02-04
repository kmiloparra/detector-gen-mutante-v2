package detector.gen.mutante.model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.google.gson.GsonBuilder;

import lombok.Data;

@Data
public class Request implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1210530614460404556L;
	
	@JsonProperty("dna")
	private String[] dna;
	
	@JsonProperty("esMutante")
	private String esMutante;

	@Override
	public String toString() {
		return new GsonBuilder().create().toJson(this);
	}
}
