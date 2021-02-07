package detector.gen.mutante.aws.function;

import java.io.IOException;
import java.util.concurrent.Future;
import java.util.function.Function;

import org.springframework.boot.SpringBootConfiguration;
import org.springframework.boot.json.JsonParseException;
import org.springframework.cloud.function.context.FunctionRegistration;
import org.springframework.cloud.function.context.FunctionType;
import org.springframework.context.ApplicationContextInitializer;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.http.HttpStatus;

import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyRequestEvent;
import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyResponseEvent;
import com.amazonaws.services.sqs.AmazonSQSAsync;
import com.amazonaws.services.sqs.AmazonSQSAsyncClientBuilder;
import com.amazonaws.services.sqs.model.SendMessageResult;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import detector.gen.mutante.cache.ManejadorRedis;
import detector.gen.mutante.constantes.Constantes;
import detector.gen.mutante.model.Request;
import detector.gen.mutante.service.BuscadorGenomicoService;
import detector.gen.mutante.service.BuscadorGenomicoServiceImpl;
import detector.gen.mutante.validacion.Validacion;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@SpringBootConfiguration
public class SpringCloudFunctionAwsApplication implements ApplicationContextInitializer<GenericApplicationContext> {

	/**
	 * 
	 * Atributo que maneja el contexto de Spring de lambda
	 * 
	 */
	public GenericApplicationContext genericApplicationContext;

	/**
	 * 
	 * Formato de log
	 * 
	 */
	private static final String FORMATO_LOG = "%s %s %s";

	/**
	 * 
	 * Funcion que registra el punto de entrada hacia la lambda y asi mismo tambien
	 * la respuesta
	 * 
	 * @return Respuesta de Funcion
	 */
	public Function<APIGatewayProxyRequestEvent, APIGatewayProxyResponseEvent> startProcess() {
		return input -> {

			APIGatewayProxyResponseEvent response = new APIGatewayProxyResponseEvent();
			Request request = null;
			try {
				log.info("Request input" + input.toString());
				request = getRequest(input);
				String value = ManejadorRedis.getValue(input.getBody());
				log.info("value input valor presente: " + value);

				if (value != null) {
					return devolverRespuestaCache(response, request, value);
				} else {
					boolean esValido = validarRequest(request);
					if (!esValido) {
						response.setStatusCode(HttpStatus.BAD_REQUEST.value());
						response.setBody(HttpStatus.BAD_REQUEST.getReasonPhrase());
						return response;
					}
					procesarDna(input, response, request);
				}

			} catch (Exception e) {
				e.printStackTrace();
				log.error(String.format(FORMATO_LOG, "Error in startProcess ", e.getMessage(), e.getCause()));

				response.setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR.value());
				response.setBody(HttpStatus.INTERNAL_SERVER_ERROR.getReasonPhrase());
			}
			enviarMensajeSQS(request.toString(), Constantes.NOMBRE_COLA);
			return response;
		};
	}

	/**
	 * 
	 * Devuelve respuesta encontrada en cache (Redis)
	 * 
	 * @param response
	 * @param request
	 * @param value
	 * @return
	 */
	private APIGatewayProxyResponseEvent devolverRespuestaCache(APIGatewayProxyResponseEvent response, Request request,
			String value) {
		if (value.equals(Boolean.TRUE.toString())) {
			response.setStatusCode(HttpStatus.OK.value());
			response.setBody(HttpStatus.OK.getReasonPhrase());
			request.setEsMutante(Boolean.TRUE.toString());
			return response;
		} else {
			response.setStatusCode(HttpStatus.FORBIDDEN.value());
			response.setBody(HttpStatus.FORBIDDEN.getReasonPhrase());
			request.setEsMutante(Boolean.FALSE.toString());
			return response;
		}
	}

	/**
	 * 
	 * Metodo encargado de procesar DNA
	 * 
	 * @param input
	 * @param response
	 * @param request
	 */
	private void procesarDna(APIGatewayProxyRequestEvent input, APIGatewayProxyResponseEvent response,
			Request request) {
		BuscadorGenomicoService buscadorGenomicoService = genericApplicationContext
				.getBean(BuscadorGenomicoService.class);
		boolean esMutante = buscadorGenomicoService.isMutant(request.getDna());
		if (esMutante) {
			response.setStatusCode(HttpStatus.OK.value());
			response.setBody(HttpStatus.OK.getReasonPhrase());
			request.setEsMutante(Boolean.TRUE.toString());
			ManejadorRedis.setValue(input.getBody(), Boolean.TRUE.toString());
		} else {
			response.setStatusCode(HttpStatus.FORBIDDEN.value());
			response.setBody(HttpStatus.FORBIDDEN.getReasonPhrase());
			request.setEsMutante(Boolean.FALSE.toString());
			ManejadorRedis.setValue(input.getBody(), Boolean.FALSE.toString());
		}
	}

	/**
	 * 
	 * Metodo que valida el DNA ingresado en caso de no cumplir con alguna de las
	 * validaciones no se procesara
	 * 
	 * @param request
	 * @return
	 */
	public boolean validarRequest(Request request) {
		return Validacion.validacionTamanioDimension(request.getDna())
				? !Validacion.validacionFilaVacia(request.getDna())
						? Validacion.validacionNxN(request.getDna()) ? Validacion.validacionDominio(request.getDna())
								: Boolean.FALSE
						: Boolean.FALSE
				: Boolean.FALSE;
	}

	/**
	 * 
	 * Metodo que mapea el request
	 * 
	 * @param input
	 * @param request
	 * @return
	 * @throws IOException
	 */
	public Request getRequest(APIGatewayProxyRequestEvent input) throws IOException {
		Request request = null;
		try {
			ObjectMapper objectMapper = new ObjectMapper();
			request = objectMapper.readValue(input.getBody(), Request.class);

		} catch (JsonParseException e) {
			throw e;
		} catch (JsonMappingException e) {
			e.printStackTrace();
			throw e;
		} catch (IOException e) {
			e.printStackTrace();
			throw e;
		}
		return request;
	}

	/**
	 * 
	 * Metodo que registra los beans usados en la lambda
	 * 
	 */
	@Override
	public void initialize(GenericApplicationContext genericApplicationContext) {
		this.genericApplicationContext = genericApplicationContext;
		genericApplicationContext.registerBean(BuscadorGenomicoService.class, BuscadorGenomicoServiceImpl::new);

		genericApplicationContext.registerBean("startProcess", FunctionRegistration.class,
				() -> new FunctionRegistration<>(startProcess()).type(FunctionType
						.from(APIGatewayProxyRequestEvent.class).to(APIGatewayProxyResponseEvent.class).getType()));
	}

	/**
	 * 
	 * Metodo que envia mensaje a la cola de peticiones para almacenar en DynamoDB
	 * 
	 * @param body
	 * @param queueName
	 * @return
	 */
	public Future<SendMessageResult> enviarMensajeSQS(String body, String queueName) {
		AmazonSQSAsync sqs = AmazonSQSAsyncClientBuilder.defaultClient();
		return sqs.sendMessageAsync(sqs.getQueueUrl(queueName).getQueueUrl(), body);
	}

}
