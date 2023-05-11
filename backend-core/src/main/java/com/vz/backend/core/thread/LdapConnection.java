package com.vz.backend.core.thread;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;

import javax.naming.Context;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.directory.*;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Scope;
import org.springframework.ldap.core.LdapTemplate;
import org.springframework.ldap.core.support.LdapContextSource;
import org.springframework.ldap.filter.AndFilter;
import org.springframework.ldap.filter.EqualsFilter;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.domain.Ldap;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.service.LdapService;
import com.vz.backend.core.service.UserService;

import lombok.extern.slf4j.Slf4j;

/**
 * @author DucND
 * @date 21 thg 7, 2020
 */
@Slf4j
@Component
@Scope("prototype")
public class LdapConnection implements Runnable {

	ObjectMapper mapper = new ObjectMapper();
	Hashtable<Object, Object> env = new Hashtable<>();

	Hashtable<Object, Object> initLDAP(Ldap info) {
		env.put(Context.INITIAL_CONTEXT_FACTORY, info.getContextFactory());
		env.put(Context.PROVIDER_URL, info.getUrl());
		env.put(Context.SECURITY_AUTHENTICATION, info.getAuthenType());
		env.put(Context.SECURITY_PRINCIPAL, info.getPrincipal());
		env.put(Context.SECURITY_CREDENTIALS, info.getPassword());
		return env;
	}

	@Autowired
	private LdapService ldapService;

	@Autowired
	private UserService userService;

	@Bean
	public LdapContextSource contextSource() {
		log.info("Init ldap template & context source");
		LdapContextSource contextSource = new LdapContextSource();
		Ldap ldap = ldapService.findActiveSt(true);
		contextSource.setUrl(ldap.getUrl());
		contextSource.setBase(ldap.getDomain());
		contextSource.setUserDn(ldap.getPrincipal());
		contextSource.setPassword(ldap.getPassword());

		contextSource.afterPropertiesSet();
		log.info("Ldap info :", ldap.getDomain());
		return contextSource;
	}

	public boolean loginLdap(String username, String password) {
		try {
			LdapTemplate ldapTemplate = new LdapTemplate(contextSource());
			AndFilter filter = new AndFilter();
			ldapTemplate.setIgnorePartialResultException(true);
			filter.and(new EqualsFilter("uid", username));
			log.info("Ldap template filter : ", filter.toString());
			return ldapTemplate.authenticate("", filter.toString(), password);
		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}
	}

	private List<User> parseAttribute(NamingEnumeration values, Ldap info) {
		List<User> userResult = new ArrayList<>();
		try {
			User item;
			PasswordEncoder encoder = new BCryptPasswordEncoder();
			String passwordDefault = encoder.encode(Constant.PASSWORD_DEFAULT);
			while (values.hasMoreElements()) {
				SearchResult result = (SearchResult) values.next();
				Attributes attribs = result.getAttributes();
				if (attribs != null) {
					item = new User();
					item.setClientId(info.getClientId());
					item.setOrg(219L);// Mặc định là Cục H05 - Khi có org Ldap thì bỏ
					item.setPosition(411L);// Mặc định là Cán bộ - Khi có trong Ldap thì bỏ
					item.setClientId(info.getClientId());
					item.setPassword(passwordDefault);
					item.setChangePass(false);
					item.setLdap(true);
					mapper.createObjectNode();
					for (NamingEnumeration ae = attribs.getAll(); ae.hasMore();) {
						Attribute atr = (Attribute) ae.next();
						String attributeID = atr.getID();
						for (NamingEnumeration vals = atr.getAll(); vals.hasMore();) {
							String value = vals.next().toString();
							switch (attributeID) {
							case "mail":
								item.setEmail(value);
								break;
							case "uid":
								item.setUserName(value);
								break;
							case "zimbraAccountStatus":
								if ("active".equals(value)) {
									item.setActive(true);
								} else {
									item.setActive(false);
								}
								break;
							case "mobile":
								item.setPhone(value);
								break;
							case "displayName":
								item.setFullName(value);
								break;
							default:
								break;
							}
						}
					}
					userResult.add(item);
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		log.info("End LDAP Sync");
		return userResult;
	}

	public void connect() {
		log.info("Start LDAP Sync");

		List<User> userResult = new ArrayList<>();
		List<Ldap> ldapList = ldapService.findByActive(true);
		if (ldapList != null && ldapList.size() > 0) {
			for (Ldap element : ldapList) {
				userResult.addAll(execute(element));
			}
			saveUser(userResult);
		}
	}

	private void saveUser(List<User> userList) {
		if (userList != null && userList.size() > 0) {
			User item;
			User temp;
			List<User> tempList = new ArrayList<>();
			for (int i = 0; i < userList.size(); i++) {
				item = userList.get(i);
				temp = userService.findByUserNameForLdap(item.getUserName(), item.getClientId());
				if (temp == null) {
					temp = item;
				} else {
					temp.setFullName(item.getFullName());
					temp.setActive(item.getActive());
					temp.setEmail(item.getEmail());
					if (temp.isLdap()) {
						if(temp.getPosition() <= 0)temp.setPosition(item.getPosition());
						if(temp.getOrg() <= 0)temp.setOrg(item.getOrg());
					}
				}

				tempList.add(temp);
			}
			userService.saveAll(tempList);
			log.info("END SYNC USER SUCCESS FOR ADD OR UPDATE");
		}
	}

	private List<User> execute(Ldap info) {
		String[] attribute = info.getAttributes().split(",");
		DirContext dctx = null;
		List<User> userResult = new ArrayList<>();
		try {
			dctx = new InitialDirContext(initLDAP(info));
			SearchControls searchCtrls = new SearchControls();
			searchCtrls.setSearchScope(SearchControls.SUBTREE_SCOPE);
			searchCtrls.setReturningAttributes(attribute);
			NamingEnumeration<?> values = dctx.search(info.getDomain(), info.getFilter(), searchCtrls);
			userResult = parseAttribute(values, info);
		} catch (Exception e) {
			e.printStackTrace();
		}
		if (null != dctx) {
			try {
				dctx.close();
				dctx = null;
			} catch (final NamingException e) {
				log.error("Error in closing ldap " + e);
			}
		}
		return userResult;
	}

	public boolean changePasswordLdap(String userName, String newPassword) {
		boolean flag = true;
		DirContext dctx = null;
		try {
			Ldap ldap = ldapService.findActiveSt(true);
			dctx = new InitialDirContext(initLDAP(ldap));
			HashMap<Integer, Attribute> hashMap = new HashMap<>();
			//Attribute uid = new BasicAttribute("uid", screenName);
			//Attribute cn = new BasicAttribute("cn", screenName);
			//Attribute sn = new BasicAttribute("sn", screenName);
			if (!newPassword.equals("")) {
				Attribute userPassword = new BasicAttribute("userPassword", newPassword);
				hashMap.put(6, userPassword);
			}
			int total = hashMap.size();
			ModificationItem[] modificationItems = new ModificationItem[total];
			int i = 0;
			for (Attribute attr : hashMap.values()) {
				modificationItems[i] = new ModificationItem(DirContext.REPLACE_ATTRIBUTE, attr);
				i++;
			}
			String finderCn = "uid=" + userName + "," + ldap.getDomain();
			dctx.modifyAttributes(finderCn, modificationItems);
			flag = true;
		} catch (Exception e) {
			e.printStackTrace();
			flag = false;
		} finally {
			if (null != dctx) {
				try {
					dctx.close();
				} catch (final NamingException e) {
					System.out.println("Error in closing ldap " + e);
				}
			}
		}
		return flag;
	}

	@Override
	public void run() {
		connect();
	}
}
