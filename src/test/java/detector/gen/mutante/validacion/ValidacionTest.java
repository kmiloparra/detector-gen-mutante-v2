package detector.gen.mutante.validacion;

import static org.junit.Assert.assertEquals;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

import org.junit.Test;

import detector.gen.mutante.constantes.Constantes;

public class ValidacionTest {

	@Test
	public void validacionNxN() {
		String[] dna = { "ATGCGA", "CAGTGC", "TTATGT", "AGAAGG", "CCCCTA", "CAGTGC" };
		assertEquals(Boolean.TRUE, Validacion.validacionNxN(dna));
	}

	@Test
	public void validacionDominio() {
		String[] dna = { "ATGCGA", "CAGTGC", "TTATGT", "AGAAGG", "CCCCTA", "CAGTGC" };
		assertEquals(Boolean.TRUE, Validacion.validacionDominio(dna));
	}

	@Test
	public void validacionFilaVacia() throws NoSuchFieldException, SecurityException, Exception {
		setFinalStatic(Constantes.class.getDeclaredField("REGEX_VACIA"), "");
		String[] dna = { "", "CAGTGC", "TTATGT", "AGAAGG", "CCCCTA", "CAGTGC" };
		assertEquals(Boolean.TRUE, Validacion.validacionFilaVacia(dna));
	}

	@Test
	public void validacionTamanioDimension() throws NoSuchFieldException, SecurityException, Exception {
		setFinalStatic(Constantes.class.getDeclaredField("TAMANIO_MAXIMO_DIMENSION"), "8");
		String[] dna = { "AGAAGG", "CAGTGC", "TTATGT", "AGAAGG", "CCCCTA", "CAGTGC" };
		assertEquals(Boolean.TRUE, Validacion.validacionTamanioDimension(dna));
	}

	public static void setFinalStatic(Field field, Object newValue) throws Exception {
		field.setAccessible(true);
		Field modifiersField = Field.class.getDeclaredField("modifiers");
		modifiersField.setAccessible(true);
		modifiersField.setInt(field, field.getModifiers() & ~Modifier.FINAL);
		field.set(null, newValue);
	}

}
