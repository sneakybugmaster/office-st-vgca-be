package com.vz.backend.core.repository;

import java.util.List;

import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.core.domain.Encryption;
import com.vz.backend.core.dto.EncryptionFieldDto;

@Repository
public interface IEncryptionRepository extends IRepository<Encryption> {

	Encryption findFirstByEncryptAndUserIdAndClientIdAndActiveTrue(String name, Long userId, Long clientId);

	Encryption findFirstByKeyAndClientIdAndActiveTrue(String key, Long clientId);

	@Query("SELECT new com.vz.backend.core.dto.EncryptionFieldDto(e.key, e.encrypt, e.userId) FROM Encryption e "
			+ "WHERE e.userId=:userId and e.encrypt IN (:fileNameList)")
	List<EncryptionFieldDto> getFileId(List<String> fileNameList, Long userId);

	List<Encryption> findByEncryptAndClientIdAndActiveTrue(String name, Long clientId);

	@Query("SELECT encry FROM Encryption encry WHERE encry.userId IN :userIds and lower(encry.encrypt) LIKE %:fileName% and encry.clientId = :clientId and encry.active is true")
	Encryption findFirstByEncryptAndUsersAndClientidAndActiveTrue(String fileName, List<Long> userIds, Long clientId);

}
