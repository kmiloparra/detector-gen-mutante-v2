package detector.gen.mutante.aws.function;

import java.io.IOException;
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
import com.amazonaws.services.sqs.AmazonSQS;
import com.amazonaws.services.sqs.AmazonSQSClientBuilder;
import com.amazonaws.services.sqs.model.SendMessageRequest;
import com.amazonaws.services.sqs.model.SendMessageResult;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.gson.Gson;

import detector.gen.mutante.constantes.Constantes;
import detector.gen.mutante.model.Request;
import detector.gen.mutante.service.BuscadorGenomicoService;
import detector.gen.mutante.service.BuscadorGenomicoServiceImpl;
import detector.gen.mutante.validacion.Validacion;

@SpringBootConfiguration
public class SpringCloudFunctionAwsApplication implements ApplicationContextInitializer<GenericApplicationContext> {

	/**
	 * 
	 * Atributo que maneja el contexto de Spring de lambda
	 * 
	 */
	private GenericApplicationContext genericApplicationContext;

	/**
	 * 
	 * Funcion que registra el punto de entrada hacia la lambda
	 * 
	 * @return
	 */
	public Function<APIGatewayProxyRequestEvent, APIGatewayProxyResponseEvent> startProcess() {
		return input -> {

			APIGatewayProxyResponseEvent response = new APIGatewayProxyResponseEvent();
			Request request = null;
			try {
				System.out.println(input);
				System.out.println(input.getBody());
				request = getRequest(input, request);
				boolean esValido = validarRequest(request);
				if (!esValido) {
					response.setStatusCode(HttpStatus.BAD_REQUEST.value());
					response.setBody(HttpStatus.BAD_REQUEST.getReasonPhrase());
					return response;
				}
				BuscadorGenomicoService buscadorGenomicoService = genericApplicationContext
						.getBean(BuscadorGenomicoService.class);
				boolean esMutante = buscadorGenomicoService.isMutant(request.getDna());
				if (esMutante) {
					response.setStatusCode(HttpStatus.OK.value());
					response.setBody(HttpStatus.OK.getReasonPhrase());
					request.setEsMutante(Boolean.TRUE.toString());
				} else {
					response.setStatusCode(HttpStatus.FORBIDDEN.value());
					response.setBody(HttpStatus.FORBIDDEN.getReasonPhrase());
					request.setEsMutante(Boolean.FALSE.toString());
				}
			} catch (Exception e) {
				e.printStackTrace();
				response.setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR.value());
				response.setBody(HttpStatus.INTERNAL_SERVER_ERROR.getReasonPhrase());
			}
			
			sendToSQS(new Gson().toJson(request), Constantes.NOMBRE_COLA);
			return response;
		};
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
	private Request getRequest(APIGatewayProxyRequestEvent input, Request request) throws IOException {
		try {
			ObjectMapper objectMapper = new ObjectMapper();
			request = objectMapper.readValue(input.getBody(), Request.class);
			request.setEsMutante(Boolean.TRUE.toString());
			System.out.println(objectMapper.writeValueAsString(request));

		} catch (JsonParseException e) {
			e.printStackTrace();
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
	 * Metodo que registra los beans usados el la lambda
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
	public SendMessageResult sendToSQS(String body, String queueName) {
		AmazonSQS sqs = AmazonSQSClientBuilder.defaultClient();
		SendMessageRequest sendMessageRequest = new SendMessageRequest()
				.withQueueUrl(sqs.getQueueUrl(queueName).getQueueUrl()).withMessageBody(body);
		return sqs.sendMessage(sendMessageRequest);
	}

}
