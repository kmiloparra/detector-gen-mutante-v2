package detector.gen.mutante.model;

import java.io.Serializable;

import com.google.gson.GsonBuilder;

public class Error implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 3106728935511268356L;

	private int statusCode;

	private String reasonPhrase;

	public Error(int statusCode, String reasonPhrase) {
		super();
		this.statusCode = statusCode;
		this.reasonPhrase = reasonPhrase;
	}

	public int getStatusCode() {
		return statusCode;
	}

	public void setStatusCode(int statusCode) {
		this.statusCode = statusCode;
	}

	public String getReasonPhrase() {
		return reasonPhrase;
	}

	public void setReasonPhrase(String reasonPhrase) {
		this.reasonPhrase = reasonPhrase;
	}

	@Override
	public String toString() {
		return new GsonBuilder().create().toJson(this);
	}
}
