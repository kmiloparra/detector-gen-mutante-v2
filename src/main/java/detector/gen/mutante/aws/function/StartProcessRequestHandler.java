package detector.gen.mutante.aws.function;

import org.springframework.cloud.function.adapter.aws.SpringBootRequestHandler;

import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyRequestEvent;
import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyResponseEvent;

public class StartProcessRequestHandler
		extends SpringBootRequestHandler<APIGatewayProxyRequestEvent, APIGatewayProxyResponseEvent> {

	public StartProcessRequestHandler() {
		super(SpringCloudFunctionAwsApplication.class);
	}
}
