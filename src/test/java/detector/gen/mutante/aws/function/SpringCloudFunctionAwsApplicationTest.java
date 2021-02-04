package detector.gen.mutante.aws.function;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.IOException;

import org.apache.http.conn.ssl.SSLInitializationException;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.util.ReflectionUtils;

import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyRequestEvent;

import detector.gen.mutante.model.Request;
import detector.gen.mutante.service.BuscadorGenomicoService;
import detector.gen.mutante.validacion.Validacion;
import javassist.tools.reflect.Reflection;

@RunWith(PowerMockRunner.class)
@PrepareForTest(Validacion.class)
public class SpringCloudFunctionAwsApplicationTest {
	
	@Test(expected = SSLInitializationException.class)
	public void startProcessValido() {
		String[] mutanteHorizontales =  { "GGGGCGTA","TCAAGTAG","CGAGTAGT","GAGAGGTC","GGGGCGTA","GAGTCGAT","AGAGTCGT","CGAGTTTT"}; 		
		Request req=new Request();
		req.setDna(mutanteHorizontales);
		
		APIGatewayProxyRequestEvent event= new APIGatewayProxyRequestEvent();
		event.setBody(req.toString());
		
		new SpringCloudFunctionAwsApplication().startProcess().apply(event);
	}
	
	@Test(expected = SSLInitializationException.class)
	public void startProcessValidoGenericMutante() throws Exception {
		String[] mutanteHorizontales =  { "GGGGCGTA","TCAAGTAG","CGAGTAGT","GAGAGGTC","GGGGCGTA","GAGTCGAT","AGAGTCGT","CGAGTTTT"}; 		
		Request req=new Request();
		req.setDna(mutanteHorizontales);
		
		GenericApplicationContext genericApplicationContext =mock(GenericApplicationContext.class);
		BuscadorGenomicoService buscadorGenomicoService = mock(BuscadorGenomicoService.class);
		when(buscadorGenomicoService.isMutant(Mockito.any())).thenReturn(true);
		
		SpringCloudFunctionAwsApplication spClFu =new SpringCloudFunctionAwsApplication();
		ReflectionTestUtils.setField(spClFu, "genericApplicationContext", genericApplicationContext, GenericApplicationContext.class);
		when(genericApplicationContext.getBean(BuscadorGenomicoService.class)).thenReturn(buscadorGenomicoService);
		
		APIGatewayProxyRequestEvent event= new APIGatewayProxyRequestEvent();
		event.setBody(req.toString());
		
		spClFu.startProcess().apply(event);
	}
	
	@Test(expected = SSLInitializationException.class)
	public void startProcessValidoGenericHumano() throws Exception {
		String[] mutanteHorizontales =  { "GGGGCGTA","TCAAGTAG","CGAGTAGT","GAGAGGTC","GGGGCGTA","GAGTCGAT","AGAGTCGT","CGAGTTTT"}; 		
		Request req=new Request();
		req.setDna(mutanteHorizontales);
		
		GenericApplicationContext genericApplicationContext =mock(GenericApplicationContext.class);
		BuscadorGenomicoService buscadorGenomicoService = mock(BuscadorGenomicoService.class);
		when(buscadorGenomicoService.isMutant(Mockito.any())).thenReturn(true);
		
		SpringCloudFunctionAwsApplication spClFu =new SpringCloudFunctionAwsApplication();
		ReflectionTestUtils.setField(spClFu, "genericApplicationContext", genericApplicationContext, GenericApplicationContext.class);
		when(genericApplicationContext.getBean(BuscadorGenomicoService.class)).thenReturn(buscadorGenomicoService);
		
		APIGatewayProxyRequestEvent event= new APIGatewayProxyRequestEvent();
		event.setBody(req.toString());
		
		spClFu.startProcess().apply(event);
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
	
	
	
}
