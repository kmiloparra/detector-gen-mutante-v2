package detector.gen.mutante.constantes;

public class Constantes {
	
	public static final String REGEX_DOMINIO = "[ACGT]*";

	public static final String REGEX_VACIA = "^$|pattern";
	
	public static final String NOMBRE_COLA = System.getenv("NOMBRE_COLA");
	
	
	public static final String ALGORITMO_UTILIZADO =System.getenv("ALGORITMO_UTILIZADO");
	
	public static final String ALGORITMO_HASH =System.getenv("ALGORITMO_HASH");
	
	public static final String ALGORITMO_INDEXOF =System.getenv("ALGORITMO_INDEXOF");
	
	
	public static final String TAMANIO_MAXIMO_DIMENSION = "8";
	
	public static final int TAMANIO_SECUENCIA_MUTANTE = 4;

	public static final int CANTIDAD_SECUENCIA_MUTANTE = 2;

	public static final String SECUENCIAS_GEN_MUTANTE = "AAAA,CCCC,GGGG,TTTT";

	public static final String SECUENCIAS_GEN_MUTANTE_HASHMAP = "AAAA:A,CCCC:C,GGGG:G,TTTT:T";

	public static final String SECUENCIAS_INVALIDAS = 
			"ACGT:1,ACTG:1,AGCT:1,AGTC:1,ATCG:1,ATGC:1,CAGT:1,CATG:1,"
			+ "CGAT:1,CGTA:1,CTAG:1,CTGA:1,GACT:1,GATC:1,GCAT:1,GCTA:1,"
			+ "GTAC:1,GTCA:1,TACG:1,TAGC:1,TCAG:1,TCGA:1,TGAC:1,TGCA:1";

}
