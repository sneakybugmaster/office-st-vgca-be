package com.vz.backend.core.auth;

import java.util.Date;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.vz.backend.core.domain.Token;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.MalformedJwtException;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.UnsupportedJwtException;
import lombok.extern.slf4j.Slf4j;

/**
 * @author DucND
 * @date Apr 15, 2020
 */
@Component
@Slf4j
public class TokenHelper {

	@Value("${configs.token.user-expire:86400000}")
	private long timeExpireUser;

	@Value("${configs.token.client-expire:86400000}")
	private long timeExpireClient;

	private static final String SECRET_KEY = "VZ#2020";

	/**
	 * Generate token from user name logged
	 *
	 * @param userName
	 * @return
	 */
	public Token generateToken(String userName) {
		Date now = new Date();
		Date expiryDate = new Date(now.getTime() + this.timeExpireUser);
		String tokenValue = Jwts.builder().setSubject(userName).setIssuedAt(now).setExpiration(expiryDate)
				.signWith(SignatureAlgorithm.HS512, SECRET_KEY).compact();
		Token tokenInfo = new Token(tokenValue, expiryDate);
		return tokenInfo;
	}

	/**
	 * Generate token client
	 *
	 * @param createDate is day that client is use system
	 * @return
	 */
	public String generateExpire(Date start, String clientName) {
		if (start == null) {
			return null;
		}
		Date now = new Date();
		Date expiryDate = new Date(start.getTime() + this.timeExpireClient);
		String tokenValue = Jwts.builder().setSubject(clientName).setIssuedAt(now).setExpiration(expiryDate)
				.signWith(SignatureAlgorithm.HS512, SECRET_KEY).compact();
		Token tokenInfo = new Token(tokenValue, expiryDate);
		return tokenInfo.getAccessToken();
	}

	/**
	 * Generate token client
	 *
	 * @param clientName
	 * @param timeStamp
	 * @return
	 */
	public boolean checkExpireClient(String token) {
		try {
			Claims claims = Jwts.parser().setSigningKey(SECRET_KEY).parseClaimsJws(token).getBody();
			Date now = new Date();
			return claims.getIssuedAt().compareTo(now) < 0 && claims.getExpiration().compareTo(now) > 0;
		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}
	}

	/**
	 * Get user name from subject of JWT Token
	 *
	 * @param token
	 * @return
	 */
	public String getUserIdFromJWT(String token) {
		Claims claims = Jwts.parser().setSigningKey(SECRET_KEY).parseClaimsJws(token).getBody();
		return claims.getSubject();
	}

	public boolean validateToken(String authToken) {
		try {
			Jwts.parser().setSigningKey(SECRET_KEY).parseClaimsJws(authToken);
			return true;
		} catch (MalformedJwtException ex) {
			log.error("Invalid JWT token");
		} catch (ExpiredJwtException ex) {
			log.error("Expired JWT token");
		} catch (UnsupportedJwtException ex) {
			log.error("Unsupported JWT token");
		} catch (IllegalArgumentException ex) {
			log.error("JWT claims string is empty.");
		}
		return false;
	}
}