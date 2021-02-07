package detector.gen.mutante.service;

import java.util.Arrays;
import java.util.HashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import detector.gen.mutante.constantes.Constantes;
import detector.gen.mutante.utilidades.Utilidades;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class BuscadorGenomicoServiceImpl implements BuscadorGenomicoService{

	private static final String[] SECUENCIAS_BUSCADAS = Constantes.SECUENCIAS_GEN_MUTANTE.split(",");

	private static final HashMap<String, String> SECUENCIAS_VALIDAS = new HashMap<String, String>(
			Arrays.stream(Constantes.SECUENCIAS_GEN_MUTANTE_HASHMAP.split(",")).map(s -> s.split(":"))
					.collect(Collectors.toMap(s -> s[0], s -> s[1])));

	private static final HashMap<String, String> SECUENCIAS_INVALIDAS = new HashMap<String, String>(
			Arrays.stream(Constantes.SECUENCIAS_INVALIDAS.split(",")).map(s -> s.split(":"))
					.collect(Collectors.toMap(s -> s[0], s -> s[1])));
	
	/**
	 * 
	 * Metodo que define si es mutante o no
	 * 
	 * @param dna
	 * @return
	 */
	public boolean isMutant(String[] dna) {
		int contadorSecuenciasMutantes = 0;
		if (Constantes.ALGORITMO_UTILIZADO.equals(Constantes.ALGORITMO_HASH)) {
			log.info("Algoritmo HASH");
			contadorSecuenciasMutantes = contarSecuenciasGenomicasHorizontalesHash(dna);
			if (contadorSecuenciasMutantes >= Constantes.CANTIDAD_SECUENCIA_MUTANTE) {
				return Boolean.TRUE;
			} else {
				contadorSecuenciasMutantes += contarSecuenciasGenomicasVerticales2(dna);
				if (contadorSecuenciasMutantes >= Constantes.CANTIDAD_SECUENCIA_MUTANTE)
					return Boolean.TRUE;
				else
					contadorSecuenciasMutantes += contarSecuenciasGenomicasDiagonales2(dna);
			}
		} else {
			log.info("Algoritmo INDEXOF");
			contadorSecuenciasMutantes = contarSecuenciasGenomicasHorizontales(dna);
			if (contadorSecuenciasMutantes >= Constantes.CANTIDAD_SECUENCIA_MUTANTE) {
				return Boolean.TRUE;
			} else {
				contadorSecuenciasMutantes += contarSecuenciasGenomicasVerticales(dna);
				if (contadorSecuenciasMutantes >= Constantes.CANTIDAD_SECUENCIA_MUTANTE)
					return Boolean.TRUE;
				else
					contadorSecuenciasMutantes += contarSecuenciasGenomicasDiagonales(dna);
			}
		}
		return contadorSecuenciasMutantes >= Constantes.CANTIDAD_SECUENCIA_MUTANTE;
	};

	/**
	 * 
	 * Metodo que cuenta las secuencias genomicas mutantes en las filas de la matriz
	 * 
	 * @param dna
	 * @param secuencias
	 * @return numero de incidencias encontradas
	 */
	public int contarSecuenciasGenomicasHorizontales(String[] dna) {
		AtomicInteger contadorGenMutante = new AtomicInteger();
		ejecutarBusquedaGenomicaCadenas(dna, contadorGenMutante);
		return contadorGenMutante.get();
	}

	/**
	 * 
	 * Metodo que cuenta las secuencias genomicas mutantes en las columnas de la
	 * matriz
	 * 
	 * @param dna
	 * @param secuencias
	 * @return numero de incidencias encontradas
	 */
	public int contarSecuenciasGenomicasVerticales(String[] dna) {

		AtomicInteger contadorGenMutante = new AtomicInteger();
		String[] dnaTranspuesta = Utilidades.pivotearMatriz(dna);
		ejecutarBusquedaGenomicaCadenas(dnaTranspuesta, contadorGenMutante);
		return contadorGenMutante.get();
	}

	/**
	 * 
	 * Metodo que cuenta las secuencias genomicas mutantes en las oblicuas de la
	 * matriz
	 * 
	 * @param dna
	 * @param secuencias
	 * @return numero de incidencias encontradas
	 */
	public int contarSecuenciasGenomicasDiagonales(String[] dna) {

		AtomicInteger contadorGenMutante = new AtomicInteger();
		String[] dnaDiagonales = Utilidades.obtenerDiagonales(dna);
		ejecutarBusquedaGenomicaCadenas(dnaDiagonales, contadorGenMutante);
		return contadorGenMutante.get();
	}

	/**
	 * 
	 * Ejecuta la busqueda de secuencias genomicas mutantes en las cadenas pasadas
	 * como parametro
	 * 
	 * @param cadenas
	 * @param secuencias
	 * @param contadorGenMutante
	 */
	private void ejecutarBusquedaGenomicaCadenas(String[] cadenas, AtomicInteger contadorGenMutante) {
		int umbralGenMutante = Constantes.CANTIDAD_SECUENCIA_MUTANTE;
		Arrays.asList(cadenas).stream().takeWhile(cadena -> contadorGenMutante.get() < umbralGenMutante)
				.forEach(cadena -> {
					Arrays.asList(SECUENCIAS_BUSCADAS).stream()
							.takeWhile(secuencia -> contadorGenMutante.get() < umbralGenMutante)
							.forEach(secuencia -> contadorGenMutante
									.addAndGet(Utilidades.encontarIncidenciasIndexOf(secuencia, cadena)));
				});
	}

	/**
	 * 
	 * Metodo que cuenta las secuencias genomicas mutantes en las filas de la matriz
	 * 
	 * @param dna
	 * @param secuencias
	 * @return numero de incidencias encontradas
	 */
	public int contarSecuenciasGenomicasHorizontalesHash(String[] dna) {
		AtomicInteger contadorGenMutante = new AtomicInteger();
		ejecutarBusquedaGenomicaCadenasHash(dna, contadorGenMutante);
		return contadorGenMutante.get();
	}

	/**
	 * 
	 * Metodo que cuenta las secuencias genomicas mutantes en las columnas de la
	 * matriz
	 * 
	 * @param dna
	 * @param secuencias
	 * @return numero de incidencias encontradas
	 */
	public int contarSecuenciasGenomicasVerticales2(String[] dna) {
		AtomicInteger contadorGenMutante = new AtomicInteger();
		String[] dnaTranspuesta = Utilidades.pivotearMatriz(dna);
		ejecutarBusquedaGenomicaCadenasHash(dnaTranspuesta, contadorGenMutante);
		return contadorGenMutante.get();
	}

	/**
	 * 
	 * Metodo que cuenta las secuencias genomicas mutantes en las oblicuas de la
	 * matriz
	 * 
	 * @param dna
	 * @param secuencias
	 * @return numero de incidencias encontradas
	 */
	public int contarSecuenciasGenomicasDiagonales2(String[] dna) {
		AtomicInteger contadorGenMutante = new AtomicInteger();
		String[] dnaDiagonales = Utilidades.obtenerDiagonales(dna);
		ejecutarBusquedaGenomicaCadenasHash(dnaDiagonales, contadorGenMutante);
		return contadorGenMutante.get();
	}

	/**
	 * 
	 * Ejecuta la busqueda de secuecias genomicas mutantes en las cadenas pasadas
	 * como parametro
	 * 
	 * @param cadenas
	 * @param secuencias
	 * @param contadorGenMutante
	 */
	private void ejecutarBusquedaGenomicaCadenasHash(String[] dna, AtomicInteger contadorGenMutante) {
		Arrays.asList(dna).stream()
		.takeWhile(cadena -> contadorGenMutante.get() < Constantes.CANTIDAD_SECUENCIA_MUTANTE)
				.forEach(cadena -> contadorGenMutante.getAndAdd(
						Utilidades.encontrarIncidenciasHash(SECUENCIAS_VALIDAS, SECUENCIAS_INVALIDAS, cadena)));
	}
	
}
