package com.vz.backend.core.exception;

import java.util.Date;
import java.util.LinkedHashMap;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

/**
 * @author DucND
 * @date Apr 14, 2020
 */
@Slf4j
@Data
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class RestResponse {

	public final static int SUCCESS = 200;
	public final static int NOT_FOUND = 404;
	public final static int BAD_REQUEST = 400;
	public final static int UNAUTHORIZED = 401;
	public final static int FORBIDDEND = 403;
	public final static int METHOD_NOT_ALLOWED = 405;
	public final static int INTERNAL_SERVER_ERROR = 500;
	public final static int CONFLICT = 409;

	private int resultCode;
	private String message;
	private Date responseTime;
	private Object data;

	public RestResponse(int resultCode, Object data) {
		this.initateResponseModel(resultCode, data);
		this.responseTime = new Date();
	}

	private void initateResponseModel(int resultCode, Object data) {
		try {
//			this.data = data;
			this.resultCode = resultCode;
			String dataMessage = getMessage(data);
			if (dataMessage != null) {
				this.message = dataMessage;
				return;
			}
			if (data instanceof CustomMessage) {
				this.message = ((CustomMessage) data).message();
				this.data = data;
				return;
			}
			switch (resultCode) {
			case RestResponse.BAD_REQUEST:
				this.message = "The path info doesn't have the right format, or a parameter or request body value doesn't have the right format";
				break;
			case RestResponse.NOT_FOUND:
				this.message = "The object referenced by the path does not exist.";
				break;
			case RestResponse.UNAUTHORIZED:
				this.message = "The request has not been applied because it lacks valid authentication credentials for the target resource";
				break;
			case RestResponse.FORBIDDEND:
				this.message = "The invoker is not authorized to invoke the operation.";
				break;
			case RestResponse.METHOD_NOT_ALLOWED:
				this.message = "The method is not one of those allowed for the path.";
				break;
			case RestResponse.INTERNAL_SERVER_ERROR:
				this.message = "The execution of the service failed in some way.";
				break;
			case RestResponse.SUCCESS:
				this.message = "Success";
				this.data = data;
				break;
			default:
				this.message = "The execution of the service failed in some way.";
				break;
			}
		} catch (Exception e) {
			log.error("initateResponseModel error: " + e);
		}
	}

	@SuppressWarnings("unchecked")
	private String getMessage(Object data) {
		if (!(data instanceof LinkedHashMap<?, ?>)) {
			return null;
		}

		try {
			LinkedHashMap<String, String> map = (LinkedHashMap<String, String>) data;
			return map.get("message");
		} catch (RuntimeException e) {
			return null;
		}
	}
}
