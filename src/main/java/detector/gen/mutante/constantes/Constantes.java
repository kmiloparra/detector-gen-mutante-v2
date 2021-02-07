package detector.gen.mutante.constantes;

public class Constantes {

	public static final String REGEX_DOMINIO = "[ACGT]*";

	public static final String REGEX_VACIA = "^$|pattern";

	public static final String NOMBRE_COLA = System.getenv("NOMBRE_COLA");

	public static final String ALGORITMO_UTILIZADO = "HASH";

	public static final String ALGORITMO_HASH = "HASH";

	public static final String ALGORITMO_INDEXOF = "INDEXOF";

	public static final String URL_REDIS = System.getenv("URL_REDIS");
	
	public static final String USAR_REDIS = System.getenv("USAR_REDIS");

	public static final int PORT_REDIS = 6379;

	public static final String TAMANIO_MAXIMO_DIMENSION = System.getenv("TAMANIO_MAXIMO_DIMENSION");;

	public static final int TAMANIO_SECUENCIA_MUTANTE = 4;

	public static final int CANTIDAD_SECUENCIA_MUTANTE = 2;

	public static final String SECUENCIAS_GEN_MUTANTE = "AAAA,CCCC,GGGG,TTTT";

	public static final String SECUENCIAS_GEN_MUTANTE_HASHMAP = "AAAA:A,CCCC:C,GGGG:G,TTTT:T";

	public static final String SECUENCIAS_INVALIDAS = "AC:1,AG:1,AT:1,CA:1,CG:1,CT:1,GA:1,GC:1,GT:1";

}
