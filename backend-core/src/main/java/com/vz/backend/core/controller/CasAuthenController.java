package com.vz.backend.core.controller;

import java.security.GeneralSecurityException;

import javax.net.ssl.SSLContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.http.config.Registry;
import org.apache.http.config.RegistryBuilder;
import org.apache.http.conn.socket.ConnectionSocketFactory;
import org.apache.http.conn.socket.PlainConnectionSocketFactory;
import org.apache.http.conn.ssl.NoopHostnameVerifier;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.impl.conn.BasicHttpClientConnectionManager;
import org.apache.http.ssl.SSLContexts;
import org.apache.http.ssl.TrustStrategy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.logout.CookieClearingLogoutHandler;
import org.springframework.security.web.authentication.logout.SecurityContextLogoutHandler;
import org.springframework.security.web.authentication.rememberme.AbstractRememberMeServices;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import com.vz.backend.core.auth.TokenHelper;
import com.vz.backend.core.domain.Token;

/**
 * @author DucND
 */
@Controller
public class CasAuthenController {

	@Value("${cas.return-login}")
	private String returnLogin;
	@Value("${cas.domain}")
	private String domain;
	@Value("${cas.logout}")
	private String logout;
	@Value("${cas.login}")
	private String login;
	@Value("${cas.validate}")
	private String validate;
	@Value("${cas.proxy-validate}")
	private String proxyValidate;
	@Value("${cas.service-validate}")
	private String serviceValidate;

	@Value("${front-end.cas-login}")
	private String frontendLogin;

	@Autowired
	private TokenHelper tokenProvider;

	@GetMapping("/cas/login")
	public String logint(@RequestParam("ticket") String ticket) throws RestClientException, GeneralSecurityException {
		String endpointUrl = validate + "service={domain}&ticket={ticket}";
		String result = restTemplate().getForObject(endpointUrl, String.class, returnLogin, ticket);
		if (result != null) {
			String[] response = result.trim().split("\n");
			if (response != null && response.length > 0) {
				if ("yes".equals(response[0])) {
					String userName = response[1];
					Token token = tokenProvider.generateToken(userName);
					String redirectUrl = frontendLogin + token.getAccessToken();
					return "redirect:" + redirectUrl;
				}
			}
		}
		return "redirect:/secured";
	}

	@GetMapping("/cas/validate")
	public String validate(@RequestParam("ticket") String ticket) throws RestClientException, GeneralSecurityException {
		String endpointUrl = serviceValidate + "service={domain}&ticket={ticket}";
		restTemplate().getForObject(endpointUrl, String.class, returnLogin, ticket);
		return "123";
	}

	@GetMapping("/logout")
	public String logout(HttpServletRequest request, HttpServletResponse response,
			SecurityContextLogoutHandler logoutHandler) {
		Authentication auth = SecurityContextHolder.getContext().getAuthentication();
		logoutHandler.logout(request, response, auth);
		new CookieClearingLogoutHandler(AbstractRememberMeServices.SPRING_SECURITY_REMEMBER_ME_COOKIE_KEY)
				.logout(request, response, auth);
		return "redirect:/cas/logout";
	}

	@GetMapping("/cas/logout")
	public String logout() throws RestClientException, GeneralSecurityException {
		return "redirect:/secured";
	}

	/**
	 * Disable check SSL in RestTemplate
	 *
	 * @return
	 * @throws GeneralSecurityException
	 */
	public RestTemplate restTemplate() throws GeneralSecurityException {

		TrustStrategy acceptingTrustStrategy = (cert, authType) -> true;
		SSLContext sslContext = SSLContexts.custom().loadTrustMaterial(null, acceptingTrustStrategy).build();
		SSLConnectionSocketFactory sslsf = new SSLConnectionSocketFactory(sslContext, NoopHostnameVerifier.INSTANCE);

		Registry<ConnectionSocketFactory> socketFactoryRegistry = RegistryBuilder.<ConnectionSocketFactory>create()
				.register("https", sslsf).register("http", new PlainConnectionSocketFactory()).build();

		BasicHttpClientConnectionManager connectionManager = new BasicHttpClientConnectionManager(
				socketFactoryRegistry);
		CloseableHttpClient httpClient = HttpClients.custom().setSSLSocketFactory(sslsf)
				.setConnectionManager(connectionManager).build();

		HttpComponentsClientHttpRequestFactory requestFactory = new HttpComponentsClientHttpRequestFactory(httpClient);

		RestTemplate restTemplate = new RestTemplate(requestFactory);

		return restTemplate;
	}

}
