package detector.gen.mutante.service;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.spy;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

import org.junit.Test;

import detector.gen.mutante.constantes.Constantes;

public class BuscadorGenomicoServiceImplIndexTest {

	
	@Test
	public void contarSecuenciasGenomicasHorizontales() throws NoSuchFieldException, SecurityException, Exception {
		
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_UTILIZADO"), "INDEXOF");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_HASH"), "HASH");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_INDEXOF"), "INDEXOF");
		setFinalStatic(Constantes.class.getDeclaredField("TAMANIO_MAXIMO_DIMENSION"), "8");
		
		BuscadorGenomicoServiceImpl buscadorGenomico = spy(BuscadorGenomicoServiceImpl.class);
		
		String[] mutanteVerticales =  { "GCGGCGTA","GCAAGTAG","pppppppT","GAGAGGTC","pppppppT","GAGTCGAE","AGAGTCGT","CGAGTTGT"}; 

		assertEquals(0, buscadorGenomico.contarSecuenciasGenomicasHorizontales(mutanteVerticales));
	}	
	
	@Test
	public void contarSecuenciasGenomicasVerticales() throws NoSuchFieldException, SecurityException, Exception {
		
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_UTILIZADO"), "INDEXOF");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_HASH"), "HASH");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_INDEXOF"), "INDEXOF");
		setFinalStatic(Constantes.class.getDeclaredField("TAMANIO_MAXIMO_DIMENSION"), "8");
		
		BuscadorGenomicoServiceImpl buscadorGenomico = spy(BuscadorGenomicoServiceImpl.class);
		
		String[] mutanteVerticales =  { "GCGGCGTA","GCAAGTAG","GGAGTAGT","GAGAGGTC","pppppppT","GAGTCGAT","AGAGTCGT","CGAGTTGT"}; 

		assertEquals(2, buscadorGenomico.contarSecuenciasGenomicasVerticales(mutanteVerticales));
		
	}
	
	@Test
	public void contarSecuenciasGenomicasDiagonales() throws NoSuchFieldException, SecurityException, Exception {
		
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_UTILIZADO"), "INDEXOF");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_HASH"), "HASH");
		setFinalStatic(Constantes.class.getDeclaredField("ALGORITMO_INDEXOF"), "INDEXOF");
		setFinalStatic(Constantes.class.getDeclaredField("TAMANIO_MAXIMO_DIMENSION"), "8");
		
		BuscadorGenomicoServiceImpl buscadorGenomico = spy(BuscadorGenomicoServiceImpl.class);
		
		String[] mutanteDiagonales =  { "GTCGAGTA", "TCGAGTAG", "CGAGTAGT", "GAGAGGTC", "pppppppp", "GAGTCGAT", "AGAGTCGT","CGAGTAGT" }; 

		assertEquals(2, buscadorGenomico.contarSecuenciasGenomicasDiagonales(mutanteDiagonales));
	}
	
	public static void setFinalStatic(Field field, Object newValue) throws Exception {
		field.setAccessible(true);
		Field modifiersField = Field.class.getDeclaredField("modifiers");
		modifiersField.setAccessible(true);
		modifiersField.setInt(field, field.getModifiers() & ~Modifier.FINAL);
		field.set(null, newValue);
	}
	
}
