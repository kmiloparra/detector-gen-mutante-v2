package detector.gen.mutante.utilidades;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;

import detector.gen.mutante.constantes.Constantes;

public class Utilidades {

	/**
	 * 
	 * Metodo para encontrar el numero necesario de secuencias en una cadena
	 * 
	 * @param secuencia
	 * @param cadena
	 * @return
	 */
	public static int encontarIncidenciasIndexOf(String secuencia, String cadena) {
		int numeroIncidencias = BigInteger.ZERO.intValue();
		int indiceInicial = BigInteger.ZERO.intValue();

		for (int i = BigInteger.ZERO.intValue(); i < cadena.length()
				&& numeroIncidencias < Constantes.CANTIDAD_SECUENCIA_MUTANTE; i++) {
			indiceInicial = cadena.substring(indiceInicial).indexOf(secuencia);
			if (indiceInicial == -BigInteger.ONE.intValue())
				break;
			else {
				numeroIncidencias++;
				i = ++indiceInicial;
			}
		}
		return numeroIncidencias;
	}

	/**
	 * 
	 * Metodo para encontrar el numero necesario de secuencias en una cadena
	 * 
	 * @param secuencia
	 * @param cadena
	 * @return
	 */
	public static int encontrarIncidenciasHash(HashMap<String, String> secuenciasValidas,
			HashMap<String, String> secuenciasinvalidas, String cadena) {

		int numeroIncidencias = 0;

		for (int i = 0; i < cadena.length() - (Constantes.TAMANIO_SECUENCIA_MUTANTE - 1); i++) {
			String subCadena = cadena.substring(i, i + Constantes.TAMANIO_SECUENCIA_MUTANTE);
			String resultado = secuenciasValidas.get(subCadena);
			if (resultado != null) {
				numeroIncidencias++;
				if (i < cadena.length() - Constantes.TAMANIO_SECUENCIA_MUTANTE
						&& (Character.toString(cadena.charAt(i + Constantes.TAMANIO_SECUENCIA_MUTANTE)))
								.equals(resultado))
					return Constantes.CANTIDAD_SECUENCIA_MUTANTE;
				i += Constantes.TAMANIO_SECUENCIA_MUTANTE;
			} else {
				resultado = secuenciasinvalidas.get(subCadena.substring(2));
				if (resultado != null)
					i += Constantes.CANTIDAD_SECUENCIA_MUTANTE;
			}
		}
		return numeroIncidencias;
	}

	/**
	 * 
	 * Metodo para pivotear matriz y obtener la matriz pivote
	 * 
	 * @param dna
	 * @return
	 */
	public static String[] pivotearMatriz(String[] dna) {
		int tamanio = dna.length;
		String[] dnaTranspuesta = new String[tamanio];

		for (int i = 0; i < dna.length; i++) {
			StringBuilder columna = new StringBuilder();
			for (int j = 0; j < dnaTranspuesta.length; j++) {
				columna.append(dna[j].charAt(i));
			}
			dnaTranspuesta[i] = columna.toString();
		}
		return dnaTranspuesta;
	}

	/**
	 * 
	 * Metodo para sacar las diagonales validas para evaluar de la matriz
	 * 
	 * @param dna
	 * @return
	 */
	public static String[] obtenerDiagonales(String[] dna) {
		ArrayList<String> diagonales = new ArrayList<String>();
		int puntoPartida = dna.length - Constantes.TAMANIO_SECUENCIA_MUTANTE;

		for (int y = 0; y <= puntoPartida; y++) {
			StringBuilder diagonalInferiorDescendente = new StringBuilder();
			StringBuilder diagonalSuperiorDescendente = new StringBuilder();
			StringBuilder diagonalInferiorAscendente = new StringBuilder();
			StringBuilder diagonalSuperiorAscendente = new StringBuilder();
			int xIterador = 0;
			int yIterador = y;
			int posicionMaxima = dna.length - 1;
			for (int x = 0; x < dna.length; x++) {
				if (y + x < dna.length) {
					diagonalInferiorDescendente.append(dna[yIterador].charAt(xIterador));
					diagonalSuperiorDescendente.append(dna[xIterador].charAt(yIterador));
					diagonalInferiorAscendente.append(dna[yIterador].charAt(posicionMaxima - xIterador));
					diagonalSuperiorAscendente.append(dna[posicionMaxima - yIterador].charAt(xIterador));
				} else
					break;
				yIterador++;
				xIterador++;
			}
			diagonales.add(diagonalInferiorDescendente.toString());
			diagonales.add(diagonalInferiorAscendente.toString());
			diagonales.add(diagonalSuperiorDescendente.toString());
			diagonales.add(diagonalSuperiorAscendente.toString());

		}
		// se eliminan las diagonales repetidas
		diagonales.remove(BigInteger.ZERO.intValue());
		diagonales.remove(BigInteger.ZERO.intValue());
		return diagonales.toArray(new String[diagonales.size()]);
	}

}
