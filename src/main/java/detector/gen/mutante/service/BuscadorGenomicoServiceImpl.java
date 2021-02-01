package detector.gen.mutante.service;

import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.HashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import detector.gen.mutante.constantes.Constantes;
import detector.gen.mutante.utilidades.Utilidades;

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
	 * Metodo que define si es mutante 
	 * 
	 * @param dna
	 * @return
	 */
	public boolean isMutant(String[] dna) {
		int contadorSecuenciasMutantes = 0;
		if (Constantes.ALGORITMO_UTILIZADO.equals(Constantes.ALGORITMO_HASH)) {
			System.out.println("Algoritmo HASH");
			contadorSecuenciasMutantes = contarSecuenciasGenomicasHorizontalesHash(dna);
			System.out.println("contadorSecuenciasMutantes Horizontales: "+contadorSecuenciasMutantes);
			if (contadorSecuenciasMutantes >= Constantes.CANTIDAD_SECUENCIA_MUTANTE) {
				return Boolean.TRUE;
			} else {
				contadorSecuenciasMutantes += contarSecuenciasGenomicasVerticales2(dna);
				System.out.println("contadorSecuenciasMutantes Horizontales+verticales: "+contadorSecuenciasMutantes);
				if (contadorSecuenciasMutantes >= Constantes.CANTIDAD_SECUENCIA_MUTANTE)
					return Boolean.TRUE;
				else
					contadorSecuenciasMutantes += contarSecuenciasGenomicasDiagonales2(dna);
			}
		} else {
			System.out.println("Algoritmo INDEXOF");
			contadorSecuenciasMutantes = contarSecuenciasGenomicasHorizontales(dna);
			System.out.println("contadorSecuenciasMutantes Horizontales: "+contadorSecuenciasMutantes);
			if (contadorSecuenciasMutantes >= Constantes.CANTIDAD_SECUENCIA_MUTANTE) {
				return Boolean.TRUE;
			} else {
				contadorSecuenciasMutantes += contarSecuenciasGenomicasVerticales(dna);
				System.out.println("contadorSecuenciasMutantes Horizontales+verticales: "+contadorSecuenciasMutantes);
				if (contadorSecuenciasMutantes >= Constantes.CANTIDAD_SECUENCIA_MUTANTE)
					return Boolean.TRUE;
				else
					contadorSecuenciasMutantes += contarSecuenciasGenomicasDiagonales(dna);
			}
		}
		System.out.println("contadorSecuenciasMutantes Horizontales+verticales+oblicuos: "+contadorSecuenciasMutantes);
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
	 * Ejecuta la busqueda de secuecias genomicas mutantes en las cadenas pasadas
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
	

	public static void main(String[] args) {
		String[] dnaMutante = { "GTCpAGTA", "TCGAGTAG", "CGAGTAGT", "GAAAGGTC", "pppppppp", "GAGTCGAT", "AGAGTCGT","CGAGTAGT" }; // no mutante
		String[] dnaHumano =  { "GTCGAGTA", "TCGAGTAG", "CGAGTAGT", "GAGAGGTC", "pppppppp", "GAGTCGAT", "AGAGTCGT","CGAGTAGT" }; //mutante
		String[] mutanteHorizontales =  { "GGGGCGTA","TCAAGTAG","CGAGTAGT","GAGAGGTC","pppppppp","GAGTCGAT","AGAGTCGT","CGAGTTTT"}; 

		int inicio = ZonedDateTime.now().getNano();

		//System.out.println("dnamutante:"+ new BuscadorGenomicoServiceImpl().isMutant(dnaMutante));
		//System.out.println("dnahumano:"+ new BuscadorGenomicoServiceImpl().isMutant(dnaHumano));
		
		System.out.println("mutanteHorizontales:"+ new BuscadorGenomicoServiceImpl().contarSecuenciasGenomicasHorizontalesHash(mutanteHorizontales));

		int tfinal = ZonedDateTime.now().getNano();

		System.out.println("tiempo gastado en microsegundos:" + (tfinal - inicio) / 1000);
	}

}
