package com.vz.backend.core.service;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.repository.IUserRepository;
import org.apache.commons.lang.ArrayUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.stereotype.Service;
import org.springframework.web.util.UriUtils;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.Encryption;
import com.vz.backend.core.dto.EncryptionFieldDto;
import com.vz.backend.core.dto.LabelValueId;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IEncryptionRepository;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.util.StringUtils;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class EncryptionService extends BaseService<Encryption> {

	@Autowired
	private IEncryptionRepository encryptRepository;

	@Autowired
	private FilesStorageService storageService;

	@Autowired
	private UserService userService;

	@Autowired
	private CategoryService categoryService;

	@Autowired
	private IUserRepository userRepository;

	@Override
	public IRepository<Encryption> getRepository() {
		return encryptRepository;
	}

	private String nameOfKeyFile = "key.txt";

	public Encryption find(String name, Long userId) {
		Encryption e = getByName(name, userId);
		if (e == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_ENCRYPTION);
		}
		return e;
	}

	public Encryption findByUsers(String name, List<Long> userIds) {
		Encryption e = getByNameWithUsers(name, userIds);
		if (e == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_ENCRYPTION);
		}
		return e;
	}

	private Encryption getByName(String name, Long userId) {
		return encryptRepository.findFirstByEncryptAndUserIdAndClientIdAndActiveTrue(name, userId, BussinessCommon.getClientId());
	}

	private Encryption getByNameWithUsers(String name, List<Long> userIds) {
		return encryptRepository.findFirstByEncryptAndUsersAndClientidAndActiveTrue(name.toLowerCase(), userIds, BussinessCommon.getClientId());
	}
	
	public List<Encryption> getByName(String name) {
		return encryptRepository.findByEncryptAndClientIdAndActiveTrue(name, BussinessCommon.getClientId());
	}

	private Encryption getByKey(String key) {
		return encryptRepository.findFirstByKeyAndClientIdAndActiveTrue(key, BussinessCommon.getClientId());
	}

	public void load(OutputStream outputStream, String name) {
		Encryption e = find(name, BussinessCommon.getUserId());
		validKey(e.getKey());
		Resource encrypt = storageService.load(e.getEncrypt(), Paths.get(Constant.ENCRYPT_FILE_PATH));
		if (encrypt == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_ENCRYPTION);
		}

		File tmp = new File(getKeyFileName());
		Resource key = createKeyFile(tmp, e.getKey());

		zip(outputStream, encrypt, key);

		if (tmp.exists()) {
			tmp.delete();
		}
	}

	private void validKey(String data) {
		if (StringUtils.isNullOrEmpty(data)) {
			throw new RestExceptionHandler(Message.ENCRYPTED_FILE_KEY_INVALID);
		}
	}

	private String getKeyFileName() {
		return FilesStorageService.parse(nameOfKeyFile);
	}

	/**
	 * Create key file from data key
	 * @param data
	 * @param fileName
	 * @return key file
	 */
	private Resource createKeyFile(File tmp, String data) {
		try {
			if (tmp.createNewFile()) {
                try (OutputStream os = new FileOutputStream(tmp)) {
                    os.write(data.getBytes());
                }
				return new UrlResource("file:///" + tmp.getCanonicalPath());
			}
			log.info("Cann't create file key");
		} catch (Exception e) {
			e.printStackTrace();
		}

		throw new RestExceptionHandler(Message.NOT_FOUND_ENCRYPTION);
	}

	public void zip(OutputStream outputStream, Resource encrypt, Resource key) {
		if (encrypt == null || key == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_ENCRYPTION);
		}
		List<Resource> rs = Arrays.asList(encrypt, key);
		zipFiles(outputStream, rs);
	}

    public void zipFiles(OutputStream outputStream, List<Resource> rs) {
        try (ZipOutputStream zipOut = new ZipOutputStream(outputStream)) {
            StringBuilder tmpName;
            Resource r = null;
            for (int i = 0; i < rs.size(); i++) {
                r = rs.get(i);
                try (InputStream fis = r.getInputStream()) {
                    tmpName = new StringBuilder();
                    tmpName.append(FilesStorageService.origin(r.getFilename()));
                    ZipEntry ze = new ZipEntry(tmpName.toString());
                    log.info("Zipping the file : {}", tmpName.toString());
                    zipOut.putNextEntry(ze);
                    byte[] tmp = new byte[4 * 1024];
                    int size = 0;
                    while ((size = fis.read(tmp)) != -1) {
                        zipOut.write(tmp, 0, size);
                    }
                    zipOut.flush();
                }
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

	
	public void delEncrypt(String[] encrypts, Long[] userIds) {
		if (ArrayUtils.isEmpty(encrypts) || ArrayUtils.isEmpty(userIds))
			return;

		for (String name : encrypts) {
			for (Long userId : userIds) {
				Encryption e = getByName(name, userId);
				if (e == null)
					continue;
				
				if (userId.equals(e.getCreateBy())) {
					storageService.delete(Paths.get(Constant.ENCRYPT_FILE_PATH), e.getEncrypt());
				}

				try {
					encryptRepository.delete(e);
				} catch (Exception e2) {
					e2.printStackTrace();
				}
			}
		}
	}

	/**
	 * Delete object and file encryption
	 * @param name
	 */
	public void delEncrypt(String name) {
		List<Encryption> rs = getByName(name);
		if (BussinessCommon.isEmptyList(rs))
			return;
		Encryption e = rs.get(0);
		storageService.delete(Paths.get(Constant.ENCRYPT_FILE_PATH), e.getEncrypt());
		for (Encryption i : rs) {
			try {
				encryptRepository.delete(i);
			} catch (Exception e2) {
				e2.printStackTrace();
			}
		}
	}

	@Override
	public Encryption save(Encryption entity) {
//		if (getByName(entity.getEncrypt(), entity.getUserId()) == null && getByKey(entity.getKey()) == null) {
		if (getByName(entity.getEncrypt(), entity.getUserId()) == null) {
			return getRepository().save(entity);
		}
		return entity;
	}

	public List<Encryption> save(List<Encryption> data) {
		if (BussinessCommon.isEmptyList(data)) {
			return Collections.emptyList();
		}

		data.forEach(Encryption::valids);
		List<Long> userIds = data.stream().map(Encryption::getUserId).distinct().collect(Collectors.toList());
		List<LabelValueId<String>> users = userService.getCertAndFullNameByUserId(userIds, null);
		userService.validCertByUserId(users);
		if (users.size() != userIds.size()) {
			throw new RestExceptionHandler(Message.ENCRYPTED_FILE_INVALID);
		}

		List<Encryption> rs = new ArrayList<>();
		data.forEach(i -> rs.add(save(i)));
		return rs;
	}

	public List<EncryptionFieldDto> getFileId(String[] fileNameList) {
		List<String> fileNames = convertFileName(fileNameList);
		return encryptRepository.getFileId(fileNames, BussinessCommon.getUserId());
	}
	
	/**
	 * Convert file name 
	 * @param fileNameList
	 * @return
	 */
	private List<String> convertFileName(String[] fileNameList) {
		List<String> rs = new ArrayList<>();
		if (ArrayUtils.isEmpty(fileNameList)) {
			return rs;
		}

		String tmp = null;
		for (String str : fileNameList) {
			tmp =  UriUtils.decode(str, "UTF8");
			rs.add(tmp);
		}
		return rs;
	}
	
	/**
	 * Delete list object and file encryption
	 * @param name
	 */
	public void delEncrypt(List<String> encFileNames) {
		if (BussinessCommon.isEmptyList(encFileNames))
			return;
		encFileNames.forEach(i -> delEncrypt(i));
	}

	private List<Long> getUserOrListVanThuBan() {
		User user = BussinessCommon.getUser();
		Category positionVanThuBan = categoryService.findByName(user.getClientId(), Constant.VAN_THU_MAIN);
		List<Long> usersCheck = new ArrayList<>();
		if (positionVanThuBan != null && user.getPositionModel() != null
				&& (user.getPositionModel().getId().longValue() == positionVanThuBan.getId().longValue())) {
			usersCheck = userRepository.getUsersByNameCategory(Constant.VAN_THU_MAIN, user.getOrg());
			if (usersCheck.isEmpty()) {
				throw new RestExceptionHandler(Message.EMPTY_CLERICAL_HANDLE);
			}
		} else {
			usersCheck.add(user.getId());
		}
		return usersCheck;
	}
}
