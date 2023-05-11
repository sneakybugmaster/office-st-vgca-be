package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.Room;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IRoomRepository extends IRepository<Room> {
	List<Room> findByOrgIdAndActive(@Param(value = "orgId") long orgId, @Param(value = "active") boolean active);
}
