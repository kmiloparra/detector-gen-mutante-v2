package detector.gen.mutante.cache;

import org.springframework.data.redis.connection.jedis.JedisConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ValueOperations;

import detector.gen.mutante.constantes.Constantes;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ManejadorRedis {

	/**
	 * 
	 * RedisTemplate es el objeto necesario para interactuar con redis
	 * 
	 */
	private static RedisTemplate<String, String> REDIS_TEMPLATE = redisTemplate();

	/**
	 * 
	 * Metodo que crea el objeto JedisConnectionFactory necesario para la
	 * configuracion de redis
	 * 
	 * @return JedisConnectionFactory
	 */
	@SuppressWarnings("deprecation")
	private static JedisConnectionFactory redisConnectionFactory() {
		JedisConnectionFactory jedisConnectionFactory = new JedisConnectionFactory();
		jedisConnectionFactory.setHostName(Constantes.URL_REDIS);
		jedisConnectionFactory.setPort(Constantes.PORT_REDIS);
		jedisConnectionFactory.getPoolConfig().setMaxIdle(10);
		jedisConnectionFactory.afterPropertiesSet();
		return jedisConnectionFactory;

	}

	/**
	 * 
	 * Metodo que crea el objeto RedisTemplate necesario para interactuar con redis
	 * 
	 * @return RedisTemplate
	 */
	private static RedisTemplate<String, String> redisTemplate() {
		RedisTemplate<String, String> template = new RedisTemplate<String, String>();
		template.setConnectionFactory(redisConnectionFactory());
		template.afterPropertiesSet();
		return template;
	}

	/**
	 * 
	 * Metodo que trae consulta si existe el valor de key en redis
	 * 
	 * @param key
	 * @return value
	 */
	public static String getValue(String key) {

		String value = null;
		ValueOperations<String, String> valueOperations = REDIS_TEMPLATE.opsForValue();
		try {

			value = Constantes.USAR_REDIS.equals(Boolean.TRUE.toString()) ? valueOperations.get(key) : null;
		} catch (Exception e) {
			log.error("Problemas al traer una llave de redis -- Error: " + e.getMessage());
		}

		return value;

	}

	/**
	 * 
	 * Metodo que almacena el valor dependiendo de una llave en redis
	 * 
	 * @param key
	 * @param value
	 */
	public static void setValue(String key, String value) {
		ValueOperations<String, String> valueOperations = REDIS_TEMPLATE.opsForValue();
		try {
			if (Constantes.USAR_REDIS.equals(Boolean.TRUE.toString()))
				valueOperations.set(key, value);
		} catch (Exception e) {
			log.error("Problemas al guardar en redis -- Error: " + e.getMessage());
		}
	}
}
