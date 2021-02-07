package detector.gen.mutante.aws.function;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

import org.apache.http.conn.ssl.SSLInitializationException;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PowerMockIgnore;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.powermock.reflect.Whitebox;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.test.util.ReflectionTestUtils;

import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyRequestEvent;
import com.amazonaws.services.sqs.AmazonSQSAsync;
import com.amazonaws.services.sqs.AmazonSQSAsyncClientBuilder;
import com.amazonaws.services.sqs.model.GetQueueUrlResult;

import detector.gen.mutante.cache.ManejadorRedis;
import detector.gen.mutante.constantes.Constantes;
import detector.gen.mutante.model.Request;
import detector.gen.mutante.service.BuscadorGenomicoService;
import detector.gen.mutante.validacion.Validacion;


@RunWith(PowerMockRunner.class)
@PrepareForTest({Validacion.class,ManejadorRedis.class,AmazonSQSAsyncClientBuilder.class})
@PowerMockIgnore("javax.management.*")
public class SpringCloudFunctionAwsApplicationTest {
	
	@Test
	public void startProcessValido() throws NoSuchFieldException, SecurityException, Exception {
		String[] mutanteHorizontales =  { "GGGGCGTA","TCAAGTAG","CGAGTAGT","GAGAGGTC","GGGGCGTA","GAGTCGAT","AGAGTCGT","CGAGTTTT"}; 
		SpringCloudFunctionAwsApplication spCl =  mock(SpringCloudFunctionAwsApplication.class);
		setFinalStatic(Constantes.class.getDeclaredField("URL_REDIS"), "URL_REDIS");
		setFinalStatic(Constantes.class.getDeclaredField("USAR_REDIS"), "true");
		setFinalStatic(Constantes.class.getDeclaredField("TAMANIO_MAXIMO_DIMENSION"), "8");
		PowerMockito.mockStatic(ManejadorRedis.class);
		Mockito.when(ManejadorRedis.getValue(Mockito.anyString())).thenReturn(null);
		
		Mockito.when(spCl.validarRequest(Mockito.any())).thenCallRealMethod();
		Mockito.when(spCl.startProcess()).thenCallRealMethod();
		Mockito.when(spCl.getRequest(Mockito.any())).thenCallRealMethod();
		
		GenericApplicationContext genericApplicationContext =mock(GenericApplicationContext.class);
		BuscadorGenomicoService buscadorGenomicoService = mock(BuscadorGenomicoService.class);
		when(buscadorGenomicoService.isMutant(Mockito.any())).thenReturn(true);
		
		ReflectionTestUtils.setField(spCl, "genericApplicationContext", genericApplicationContext, GenericApplicationContext.class);
		when(genericApplicationContext.getBean(BuscadorGenomicoService.class)).thenReturn(buscadorGenomicoService);
		
		
		Request req=new Request();
		req.setDna(mutanteHorizontales);
		
		
		APIGatewayProxyRequestEvent event= new APIGatewayProxyRequestEvent();
		event.setBody(req.toString());
	
		
		assertEquals((Integer)200, spCl.startProcess().apply(event).getStatusCode());
	}
	
	@Test
	public void startProcessValidoGenericMutante() throws Exception {
		String[] mutanteHorizontales =  { "GTGTCGTA","TCAAGTAG","CGAGTAGT","GAGAGGTC","TGTGCGTA","GAGTCGAT","AGAGTCGT","CGAGTCTC"}; 
		SpringCloudFunctionAwsApplication spCl =  mock(SpringCloudFunctionAwsApplication.class);
		setFinalStatic(Constantes.class.getDeclaredField("URL_REDIS"), "URL_REDIS");
		setFinalStatic(Constantes.class.getDeclaredField("USAR_REDIS"), "true");
		setFinalStatic(Constantes.class.getDeclaredField("TAMANIO_MAXIMO_DIMENSION"), "8");
		PowerMockito.mockStatic(ManejadorRedis.class);
		Mockito.when(ManejadorRedis.getValue(Mockito.anyString())).thenReturn(null);
		
		Mockito.when(spCl.validarRequest(Mockito.any())).thenCallRealMethod();
		Mockito.when(spCl.startProcess()).thenCallRealMethod();
		Mockito.when(spCl.getRequest(Mockito.any())).thenCallRealMethod();
		
		GenericApplicationContext genericApplicationContext =mock(GenericApplicationContext.class);
		BuscadorGenomicoService buscadorGenomicoService = mock(BuscadorGenomicoService.class);
		when(buscadorGenomicoService.isMutant(Mockito.any())).thenReturn(true);
		
		ReflectionTestUtils.setField(spCl, "genericApplicationContext", genericApplicationContext, GenericApplicationContext.class);
		when(genericApplicationContext.getBean(BuscadorGenomicoService.class)).thenReturn(buscadorGenomicoService);
		
		Request req=new Request();
		req.setDna(mutanteHorizontales);
		
		
		APIGatewayProxyRequestEvent event= new APIGatewayProxyRequestEvent();
		event.setBody(req.toString());
	
		
		assertEquals((Integer)200, spCl.startProcess().apply(event).getStatusCode());
	}
	
	@Test
	public void startProcessValidoGenericHumano() throws Exception {
		String[] mutanteHorizontales =  { "GTCTAGTA","TCGAGTAG","CGTGTAGT","GAAAGATC","CGATTAGG","GAGTCGAT","AGAGTCTT","CGAGTAGT"}; 
		SpringCloudFunctionAwsApplication spCl =  mock(SpringCloudFunctionAwsApplication.class);
		setFinalStatic(Constantes.class.getDeclaredField("URL_REDIS"), "URL_REDIS");
		setFinalStatic(Constantes.class.getDeclaredField("USAR_REDIS"), "true");
		setFinalStatic(Constantes.class.getDeclaredField("TAMANIO_MAXIMO_DIMENSION"), "8");
		PowerMockito.mockStatic(ManejadorRedis.class);
		Mockito.when(ManejadorRedis.getValue(Mockito.anyString())).thenReturn(null);
		
		Mockito.when(spCl.validarRequest(Mockito.any())).thenCallRealMethod();
		Mockito.when(spCl.startProcess()).thenCallRealMethod();
		Mockito.when(spCl.getRequest(Mockito.any())).thenCallRealMethod();
		
		GenericApplicationContext genericApplicationContext =mock(GenericApplicationContext.class);
		BuscadorGenomicoService buscadorGenomicoService = mock(BuscadorGenomicoService.class);
		when(buscadorGenomicoService.isMutant(Mockito.any())).thenReturn(false);
		
		ReflectionTestUtils.setField(spCl, "genericApplicationContext", genericApplicationContext, GenericApplicationContext.class);
		when(genericApplicationContext.getBean(BuscadorGenomicoService.class)).thenReturn(buscadorGenomicoService);
		
		Request req=new Request();
		req.setDna(mutanteHorizontales);
		
		
		APIGatewayProxyRequestEvent event= new APIGatewayProxyRequestEvent();
		event.setBody(req.toString());
	
		
		assertEquals((Integer)403, spCl.startProcess().apply(event).getStatusCode());
	}
	
	@Test
	public void startProcessRedisGetTrue() throws Exception {
		String[] mutanteHorizontales =  { "GTCTAGTA","TCGAGTAG","CGTGTAGT","GAAAGATC","CGATTAGG","GAGTCGAT","AGAGTCTT","CGAGTAGT"}; 
		SpringCloudFunctionAwsApplication spCl =  mock(SpringCloudFunctionAwsApplication.class);
		setFinalStatic(Constantes.class.getDeclaredField("URL_REDIS"), "URL_REDIS");
		setFinalStatic(Constantes.class.getDeclaredField("USAR_REDIS"), "true");
		setFinalStatic(Constantes.class.getDeclaredField("TAMANIO_MAXIMO_DIMENSION"), "8");
		PowerMockito.mockStatic(ManejadorRedis.class);
		Mockito.when(ManejadorRedis.getValue(Mockito.anyString())).thenReturn("true");
		
		Mockito.when(spCl.validarRequest(Mockito.any())).thenCallRealMethod();
		Mockito.when(spCl.startProcess()).thenCallRealMethod();
		Mockito.when(spCl.getRequest(Mockito.any())).thenCallRealMethod();
		
		GenericApplicationContext genericApplicationContext =mock(GenericApplicationContext.class);
		BuscadorGenomicoService buscadorGenomicoService = mock(BuscadorGenomicoService.class);
		when(buscadorGenomicoService.isMutant(Mockito.any())).thenReturn(false);
		
		ReflectionTestUtils.setField(spCl, "genericApplicationContext", genericApplicationContext, GenericApplicationContext.class);
		when(genericApplicationContext.getBean(BuscadorGenomicoService.class)).thenReturn(buscadorGenomicoService);
		
		Request req=new Request();
		req.setDna(mutanteHorizontales);
		
		
		APIGatewayProxyRequestEvent event= new APIGatewayProxyRequestEvent();
		event.setBody(req.toString());
	
		
		assertEquals((Integer)200, spCl.startProcess().apply(event).getStatusCode());
	}
	
	@Test
	public void startProcessException() throws Exception {
		String[] mutanteHorizontales =  { "GTCTAGTA","TCGAGTAG","CGTGTAGT","GAAAGATC","CGATTAGG","GAGTCGAT","AGAGTCTT","CGAGTAGT"}; 
		SpringCloudFunctionAwsApplication spCl =  mock(SpringCloudFunctionAwsApplication.class);
		setFinalStatic(Constantes.class.getDeclaredField("URL_REDIS"), "URL_REDIS");
		setFinalStatic(Constantes.class.getDeclaredField("USAR_REDIS"), "true");
		setFinalStatic(Constantes.class.getDeclaredField("TAMANIO_MAXIMO_DIMENSION"), "8");
		PowerMockito.mockStatic(ManejadorRedis.class);
		Mockito.when(ManejadorRedis.getValue(Mockito.anyString())).thenReturn(null);
		
		Mockito.when(spCl.validarRequest(Mockito.any())).thenCallRealMethod();
		Mockito.when(spCl.startProcess()).thenCallRealMethod();
		Mockito.when(spCl.getRequest(Mockito.any())).thenCallRealMethod();
		
		GenericApplicationContext genericApplicationContext =mock(GenericApplicationContext.class);
		BuscadorGenomicoService buscadorGenomicoService = mock(BuscadorGenomicoService.class);
		when(buscadorGenomicoService.isMutant(Mockito.any())).thenReturn(false);
		
		ReflectionTestUtils.setField(spCl, "genericApplicationContext", genericApplicationContext, GenericApplicationContext.class);
		when(genericApplicationContext.getBean(BuscadorGenomicoService.class)).thenReturn(null);
		
		Request req=new Request();
		req.setDna(mutanteHorizontales);
		
		
		APIGatewayProxyRequestEvent event= new APIGatewayProxyRequestEvent();
		event.setBody(req.toString());
	
		
		assertEquals((Integer)500, spCl.startProcess().apply(event).getStatusCode());
	}
	
	@Test
	public void startProcessRedisGetFalse() throws Exception {
		String[] mutanteHorizontales =  { "GTCTAGTA","TCGAGTAG","CGTGTAGT","GAAAGATC","CGATTAGG","GAGTCGAT","AGAGTCTT","CGAGTAGT"}; 
		SpringCloudFunctionAwsApplication spCl =  mock(SpringCloudFunctionAwsApplication.class);
		setFinalStatic(Constantes.class.getDeclaredField("URL_REDIS"), "URL_REDIS");
		setFinalStatic(Constantes.class.getDeclaredField("USAR_REDIS"), "true");
		setFinalStatic(Constantes.class.getDeclaredField("TAMANIO_MAXIMO_DIMENSION"), "8");
		PowerMockito.mockStatic(ManejadorRedis.class);
		Mockito.when(ManejadorRedis.getValue(Mockito.anyString())).thenReturn("false");
		
		Mockito.when(spCl.validarRequest(Mockito.any())).thenCallRealMethod();
		Mockito.when(spCl.startProcess()).thenCallRealMethod();
		Mockito.when(spCl.getRequest(Mockito.any())).thenCallRealMethod();
		
		GenericApplicationContext genericApplicationContext =mock(GenericApplicationContext.class);
		BuscadorGenomicoService buscadorGenomicoService = mock(BuscadorGenomicoService.class);
		when(buscadorGenomicoService.isMutant(Mockito.any())).thenReturn(false);
		
		ReflectionTestUtils.setField(spCl, "genericApplicationContext", genericApplicationContext, GenericApplicationContext.class);
		when(genericApplicationContext.getBean(BuscadorGenomicoService.class)).thenReturn(buscadorGenomicoService);
		
		Request req=new Request();
		req.setDna(mutanteHorizontales);
		
		APIGatewayProxyRequestEvent event= new APIGatewayProxyRequestEvent();
		event.setBody(req.toString());
	
		assertEquals((Integer)403, spCl.startProcess().apply(event).getStatusCode());
	}
	
	@Test
	public void startProcessInvalido() {
		String[] mutanteHorizontales =  { "GGGGCGTA","TCAAGTAG","CGAGTAGT","GAGAGGTC","pppppppp","GAGTCGAT","AGAGTCGT","CGAGTTTT"}; 		
		Request req=new Request();
		req.setDna(mutanteHorizontales);
		
		APIGatewayProxyRequestEvent event= new APIGatewayProxyRequestEvent();
		event.setBody(req.toString());
		
		new SpringCloudFunctionAwsApplication().startProcess().apply(event);
	}

	@Test
	public void validarRequest() {
		PowerMockito.mockStatic(Validacion.class);
		
		String[] dna= {};		
		Request req=new Request();
		req.setDna(dna);
		req.hashCode();
		req.equals(null);
		Mockito.when(Validacion.validacionTamanioDimension(dna)).thenReturn(true);
		Mockito.when(Validacion.validacionFilaVacia(dna)).thenReturn(false);
		Mockito.when(Validacion.validacionNxN(dna)).thenReturn(true);
		Mockito.when(Validacion.validacionDominio(dna)).thenReturn(true);
		assertEquals(true, new SpringCloudFunctionAwsApplication().validarRequest(req));
		
		Mockito.when(Validacion.validacionTamanioDimension(dna)).thenReturn(false);
		Mockito.when(Validacion.validacionFilaVacia(dna)).thenReturn(false);
		Mockito.when(Validacion.validacionNxN(dna)).thenReturn(true);
		Mockito.when(Validacion.validacionDominio(dna)).thenReturn(true);

		assertEquals(false, new SpringCloudFunctionAwsApplication().validarRequest(req));
		
		Mockito.when(Validacion.validacionTamanioDimension(dna)).thenReturn(true);
		Mockito.when(Validacion.validacionFilaVacia(dna)).thenReturn(true);
		Mockito.when(Validacion.validacionNxN(dna)).thenReturn(true);
		Mockito.when(Validacion.validacionDominio(dna)).thenReturn(true);

		assertEquals(false, new SpringCloudFunctionAwsApplication().validarRequest(req));
		
		Mockito.when(Validacion.validacionTamanioDimension(dna)).thenReturn(true);
		Mockito.when(Validacion.validacionFilaVacia(dna)).thenReturn(true);
		Mockito.when(Validacion.validacionNxN(dna)).thenReturn(false);
		Mockito.when(Validacion.validacionDominio(dna)).thenReturn(true);

		assertEquals(false, new SpringCloudFunctionAwsApplication().validarRequest(req));
		
		Mockito.when(Validacion.validacionTamanioDimension(dna)).thenReturn(true);
		Mockito.when(Validacion.validacionFilaVacia(dna)).thenReturn(true);
		Mockito.when(Validacion.validacionNxN(dna)).thenReturn(true);
		Mockito.when(Validacion.validacionDominio(dna)).thenReturn(false);

		assertEquals(false, new SpringCloudFunctionAwsApplication().validarRequest(req));
		
		Mockito.when(Validacion.validacionTamanioDimension(dna)).thenReturn(true);
		Mockito.when(Validacion.validacionFilaVacia(dna)).thenReturn(false);
		Mockito.when(Validacion.validacionNxN(dna)).thenReturn(false);
		Mockito.when(Validacion.validacionDominio(dna)).thenReturn(true);

		assertEquals(false, new SpringCloudFunctionAwsApplication().validarRequest(req));
		
		
		
	}
	
	@Test
	public void getRequest() throws IOException {
		String[] dna= {};		
		Request req=new Request();
		req.setDna(dna);
		new StartProcessRequestHandler();
		APIGatewayProxyRequestEvent input = new APIGatewayProxyRequestEvent();
		
		input.setBody(req.toString());
		assertEquals(req, new SpringCloudFunctionAwsApplication().getRequest(input));
	}
	
	@Test(expected = com.fasterxml.jackson.core.JsonParseException.class)
	public void getRequestJsonParseException() throws IOException {
		String[] dna= {};		
		Request req=new Request();
		req.setDna(dna);
		APIGatewayProxyRequestEvent input = new APIGatewayProxyRequestEvent();
		
		input.setBody("{5}");
		assertEquals(req, new SpringCloudFunctionAwsApplication().getRequest(input));
	}
	
	@Test(expected = com.fasterxml.jackson.core.JsonParseException.class)
	public void getRequestJsonMappingException() throws IOException {
		String[] dna= {};		
		Request req=new Request();
		req.setDna(dna);
		APIGatewayProxyRequestEvent input = new APIGatewayProxyRequestEvent();
		
		input.setBody("{nombre:rere}");
		assertEquals(req, new SpringCloudFunctionAwsApplication().getRequest(input));
	}
	
	@Test
	public void redis() throws Exception {
		
		RedisTemplate<String, String> template= Whitebox.invokeMethod(ManejadorRedis.class, "redisTemplate");
		ManejadorRedis.getValue("");
		ManejadorRedis.setValue("", "");
		assertNotNull(template);
	}
	
	@Test
	public void initialize() {
		GenericApplicationContext genericApplicationContext =mock(GenericApplicationContext.class);
		new SpringCloudFunctionAwsApplication().initialize(genericApplicationContext);
		PowerMockito.mockStatic(AmazonSQSAsyncClientBuilder.class);
		AmazonSQSAsync sqs = mock(AmazonSQSAsync.class);
		new Constantes();
		GetQueueUrlResult queueName = mock(GetQueueUrlResult.class);
		when(sqs.getQueueUrl(Mockito.anyString())).thenReturn(queueName);
		when(sqs.sendMessageAsync(Mockito.anyString(),Mockito.anyString())).thenReturn(null);
		when(queueName.getQueueUrl()).thenReturn("");
		when(AmazonSQSAsyncClientBuilder.defaultClient()).thenReturn(sqs);
		assertEquals(null, new SpringCloudFunctionAwsApplication().enviarMensajeSQS("", ""));
	}
	
	public static void setFinalStatic(Field field, Object newValue) throws Exception {
		field.setAccessible(true);
		Field modifiersField = Field.class.getDeclaredField("modifiers");
		modifiersField.setAccessible(true);
		modifiersField.setInt(field, field.getModifiers() & ~Modifier.FINAL);
		field.set(null, newValue);
	}
	
}
