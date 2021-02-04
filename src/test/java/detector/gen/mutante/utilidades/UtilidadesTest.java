package detector.gen.mutante.utilidades;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.HashMap;
import java.util.stream.Collectors;

import org.junit.Test;

import detector.gen.mutante.constantes.Constantes;

public class UtilidadesTest {

	private static final String[] SECUENCIAS_BUSCADAS = Constantes.SECUENCIAS_GEN_MUTANTE.split(",");

	private static final HashMap<String, String> SECUENCIAS_VALIDAS = new HashMap<String, String>(
			Arrays.stream(Constantes.SECUENCIAS_GEN_MUTANTE_HASHMAP.split(",")).map(s -> s.split(":"))
					.collect(Collectors.toMap(s -> s[0], s -> s[1])));

	private static final HashMap<String, String> SECUENCIAS_INVALIDAS = new HashMap<String, String>(
			Arrays.stream(Constantes.SECUENCIAS_INVALIDAS.split(",")).map(s -> s.split(":"))
					.collect(Collectors.toMap(s -> s[0], s -> s[1])));

	@Test
	public void encontarIncidenciasIndexOf() {
		assertEquals(1, Utilidades.encontarIncidenciasIndexOf("TTTT", "CGAGTTTT"));
		assertEquals(1, Utilidades.encontarIncidenciasIndexOf("GGGG", "GGGGCGTA"));
		assertEquals(0, Utilidades.encontarIncidenciasIndexOf("TTTT", "CGAGTTTA"));
	}

	@Test
	public void encontarIncidencia() {
//		assertEquals(1, Utilidades.encontrarIncidenciasHash(SECUENCIAS_VALIDAS, SECUENCIAS_INVALIDAS, "CGAGTTTT"));
//		assertEquals(0, Utilidades.encontrarIncidenciasHash(SECUENCIAS_VALIDAS, SECUENCIAS_INVALIDAS, "CGAGTTTA"));
	}

	@Test
	public void pivotearMatriz() {
		String[] dna = { "GGGGCGTA", "TCAAGTAG", "CGAGTAGT", "GAGAGGTC", "pppppppp", "GAGTCGAT", "AGAGTCGT",
				"CGAGTTTT" };
		String[] dnaPivote = Utilidades.pivotearMatriz(dna);
		for (int i = 0; i < dna.length; i++) {
			for (int j = 0; j < dnaPivote.length; j++) {
				assertEquals(dna[i].charAt(j), dnaPivote[j].charAt(i));
			}
		}
	}

	@Test
	public void obtenerDiagonales() {
		String[] dna = { "GGGGCGTA", "TCAAGTAG", "CGAGTAGT", "GAGAGGTC", "pppppppp", "GAGTCGAT", "AGAGTCGT",
				"CGAGTTTT" };
		String[] diagonales = { "GCAApGGT", "CGGpGAAA", "TGGpCCT", "GGGpTAG", "GAGGpAT", "AApATTT", "CApTTT", "TTpCGA",
				"GATGpT", "GpGGGG", "GpGGT", "CpGTG", "GGATp", "pAAAC", "pAAG", "pACT", "CTGC", "GGAG" };
		String[] cadenasDiagonales = Utilidades.obtenerDiagonales(dna);
		assertEquals(Boolean.TRUE, Arrays.asList(diagonales).stream()
				.allMatch(diagonal -> Arrays.asList(cadenasDiagonales).contains(diagonal)));

	}

}
