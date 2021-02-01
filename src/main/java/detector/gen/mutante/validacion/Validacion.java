package detector.gen.mutante.validacion;

import java.util.Arrays;
import java.util.regex.Pattern;
import detector.gen.mutante.constantes.Constantes;

public class Validacion {
	/**
	 * Valida la dimension de la matriz si es cuadrada (la misma cantidad de filas
	 * por columnas)
	 * 
	 * @param dna
	 * @return
	 */
	public static boolean validacionNxN(String[] dna) {
		int numeroFilas = dna.length;
		return !Arrays.asList(dna).stream().anyMatch(fila -> fila.length() != numeroFilas);
	}

	/**
	 * Valida que los elementos de la matriz solo tengan valores validos
	 * 
	 * @param dna
	 * @return
	 */
	public static boolean validacionDominio(String[] dna) {
		return Arrays.asList(dna).stream()
				.allMatch(fila -> Pattern.matches(Constantes.REGEX_DOMINIO, fila.toUpperCase()));
	}

	/**
	 * Valida si hay al menos una fila vacia de la matriz
	 * 
	 * @param dna
	 * @return
	 */
	public static boolean validacionFilaVacia(String[] dna) {
		return Arrays.asList(dna).stream().anyMatch(fila -> Pattern.matches(Constantes.REGEX_VACIA, fila));
	}

	/**
	 * Valida el tamanio de la dimension de la matriz no puede pasar el tope
	 * parametrizado
	 * 
	 * @param dna
	 * @return
	 */
	public static boolean validacionTamanioDimension(String[] dna) {
		return dna.length <= Integer.parseInt(Constantes.TAMANIO_MAXIMO_DIMENSION);
	}

}
