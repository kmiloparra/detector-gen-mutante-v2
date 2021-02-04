package detector.gen.mutante.service;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.spy;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

import org.junit.Test;

import detector.gen.mutante.constantes.Constantes;

public class BuscadorGenomicoServiceImplTest {
	
	
	@Test
	public void esHumanoTest() throws NoSuchFieldException, SecurityException, Exception {
		
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_UTILIZADO"), "HASH");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_HASH"), "HASH");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_INDEXOF"), "HASH");
		setFinalStatic(Constantes.class.getDeclaredField("TAMANIO_MAXIMO_DIMENSION"), "8");
		
		BuscadorGenomicoServiceImpl buscadorGenomico = spy(BuscadorGenomicoServiceImpl.class);
		
		String[] dnaNoMutante = { "GTCpAGTA", "TCGAGTAG", "CGAGTAGT", "GAAAGGTC", "pppppppp", "GAGTCGAT", "AGAGTCGT","CGAGTAGT" }; // no mutante

		assertEquals(Boolean.FALSE, buscadorGenomico.isMutant(dnaNoMutante));
	}
	
	@Test
	public void esMutantePorHorizontalesTest() throws NoSuchFieldException, SecurityException, Exception {
		
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_UTILIZADO"), "HASH");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_HASH"), "HASH");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_INDEXOF"), "HASH");
		setFinalStatic(Constantes.class.getDeclaredField("TAMANIO_MAXIMO_DIMENSION"), "8");
		
		BuscadorGenomicoServiceImpl buscadorGenomico = spy(BuscadorGenomicoServiceImpl.class);
		
		String[] mutanteHorizontales =  { "GGGGCGTA","TCAAGTAG","CGAGTAGT","GAGAGGTC","pppppppp","GAGTCGAT","AGAGTCGT","CGAGTTTT"}; 

		assertEquals(Boolean.TRUE, buscadorGenomico.isMutant(mutanteHorizontales));
	}
	
	@Test
	public void noEsMutantePorHorizontalesTest() throws NoSuchFieldException, SecurityException, Exception {
		
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_UTILIZADO"), "HASH");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_HASH"), "HASH");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_INDEXOF"), "HASH");
		setFinalStatic(Constantes.class.getDeclaredField("TAMANIO_MAXIMO_DIMENSION"), "8");
		
		BuscadorGenomicoServiceImpl buscadorGenomico = spy(BuscadorGenomicoServiceImpl.class);
		
		String[] mutanteHorizontales =  { "GEGGCGTA","TCAAGTAG","CGAGTAGT","pppppppp","GAGAGGTC","GAGTCGAT","AGAGTCGT","CGAGTTTT"}; 

		assertEquals(Boolean.FALSE, buscadorGenomico.isMutant(mutanteHorizontales));
	}
	
	@Test
	public void esMutantePorVerticalesTest() throws NoSuchFieldException, SecurityException, Exception {
		
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_UTILIZADO"), "HASH");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_HASH"), "HASH");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_INDEXOF"), "INDEXOF");
		setFinalStatic(Constantes.class.getDeclaredField("TAMANIO_MAXIMO_DIMENSION"), "8");
		
		BuscadorGenomicoServiceImpl buscadorGenomico = spy(BuscadorGenomicoServiceImpl.class);
		
		String[] mutanteVerticales =  { "GCGGCGTA","GCAAGTAG","GGAGTAGT","GAGAGGTC","pppppppT","GAGTCGAT","AGAGTCGT","CGAGTTGT"}; 

		assertEquals(Boolean.TRUE, buscadorGenomico.isMutant(mutanteVerticales));
		
	}
	
	@Test
	public void esMutantePorVerticalesindexTest() throws NoSuchFieldException, SecurityException, Exception {
		
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_UTILIZADO"), "INDEXOF");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_HASH"), "HASH");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_INDEXOF"), "INDEXOF");
		setFinalStatic(Constantes.class.getDeclaredField("TAMANIO_MAXIMO_DIMENSION"), "8");
		
		BuscadorGenomicoServiceImpl buscadorGenomico = spy(BuscadorGenomicoServiceImpl.class);
		
		String[] mutanteVerticales =  { "GCGGCGTA","GCAAGTAG","GGAGTAGT","GAGAGGTC","pppppppT","GAGTCGAT","AGAGTCGT","CGAGTTGT"}; 

		assertEquals(Boolean.TRUE, buscadorGenomico.isMutant(mutanteVerticales));
		
	}
	
	
	
	@Test
	public void noEsMutantePorVerticalesTest() throws NoSuchFieldException, SecurityException, Exception {
		
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_UTILIZADO"), "HASH");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_HASH"), "HASH");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_INDEXOF"), "INDEXOF");
		setFinalStatic(Constantes.class.getDeclaredField("TAMANIO_MAXIMO_DIMENSION"), "8");
		
		BuscadorGenomicoServiceImpl buscadorGenomico = spy(BuscadorGenomicoServiceImpl.class);
		
		String[] mutanteVerticales =  { "GCGGCGTA","GCAAGTAG","pppppppT","GAGAGGTC","pppppppT","GAGTCGAE","AGAGTCGT","CGAGTTGT"}; 

		assertEquals(Boolean.FALSE, buscadorGenomico.isMutant(mutanteVerticales));
	}
	
	@Test
	public void noEsMutantePorVerticalesIndexTest() throws NoSuchFieldException, SecurityException, Exception {
		
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_UTILIZADO"), "INDEXOF");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_HASH"), "HASH");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_INDEXOF"), "INDEXOF");
		setFinalStatic(Constantes.class.getDeclaredField("TAMANIO_MAXIMO_DIMENSION"), "8");
		
		
		BuscadorGenomicoServiceImpl buscadorGenomico = spy(BuscadorGenomicoServiceImpl.class);
		
		String[] mutanteVerticales =  { "GCGGCGTA","GCAAGTAG","pppppppT","GAGAGGTC","pppppppT","GAGTCGAE","AGAGTCGT","CGAGTTGT"}; 

		assertEquals(Boolean.FALSE, buscadorGenomico.isMutant(mutanteVerticales));
	}
	
	@Test
	public void esMutantePorDiagonalesTest() throws NoSuchFieldException, SecurityException, Exception {
		
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_UTILIZADO"), "HASH");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_HASH"), "HASH");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_INDEXOF"), "INDEXOF");
		setFinalStatic(Constantes.class.getDeclaredField("TAMANIO_MAXIMO_DIMENSION"), "8");
		
		BuscadorGenomicoServiceImpl buscadorGenomico = spy(BuscadorGenomicoServiceImpl.class);
		
		String[] mutanteDiagonales =  { "GTCGAGTA", "TCGAGTAG", "CGAGTAGT", "GAGAGGTC", "pppppppp", "GAGTCGAT", "AGAGTCGT","CGAGTAGT" }; 

		assertEquals(Boolean.TRUE, buscadorGenomico.isMutant(mutanteDiagonales));
	}
	
	@Test
	public void esMutantePorDiagonalesIndexTest() throws NoSuchFieldException, SecurityException, Exception {
		
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_UTILIZADO"), "INDEXOF");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_HASH"), "HASH");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_INDEXOF"), "INDEXOF");
		setFinalStatic(Constantes.class.getDeclaredField("TAMANIO_MAXIMO_DIMENSION"), "8");
		
		BuscadorGenomicoServiceImpl buscadorGenomico = spy(BuscadorGenomicoServiceImpl.class);
		
		String[] mutanteDiagonales =  { "GTCGAGTA", "TCGAGTAG", "CGAGTAGT", "GAGAGGTC", "pppppppp", "GAGTCGAT", "AGAGTCGT","CGAGTAGT" }; 

		assertEquals(Boolean.TRUE, buscadorGenomico.isMutant(mutanteDiagonales));
	}
	
	@Test
	public void noEsMutantePorDiagonalesTest() throws NoSuchFieldException, SecurityException, Exception {
		
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_UTILIZADO"), "HASH");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_HASH"), "HASH");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_INDEXOF"), "HASH");
		setFinalStatic(Constantes.class.getDeclaredField("TAMANIO_MAXIMO_DIMENSION"), "8");
		
		BuscadorGenomicoServiceImpl buscadorGenomico = spy(BuscadorGenomicoServiceImpl.class);
		
		String[] mutanteDiagonales =  { "GTCGAGTA", "pppppppp", "CGAGTAGT", "GAGAGGTC", "pppppppp", "GAGTCGAT", "AGAGTCGT","CGAGTAGT" }; 

		assertEquals(Boolean.FALSE, buscadorGenomico.isMutant(mutanteDiagonales));
	}
	
	public static void setFinalStatic(Field field, Object newValue) throws Exception {
		field.setAccessible(true);
		Field modifiersField = Field.class.getDeclaredField("modifiers");
		modifiersField.setAccessible(true);
		modifiersField.setInt(field, field.getModifiers() & ~Modifier.FINAL);
		field.set(null, newValue);
	}

}
