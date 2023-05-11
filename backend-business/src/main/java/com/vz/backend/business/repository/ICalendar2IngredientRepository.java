package com.vz.backend.business.repository;

import com.vz.backend.business.config.ReceiveTypeEnum;
import com.vz.backend.business.domain.Calendar2Ingredient;
import com.vz.backend.core.dto.OrgGroupDto;
import com.vz.backend.core.dto.UserBasicDto;
import com.vz.backend.core.repository.IRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ICalendar2IngredientRepository extends IRepository<Calendar2Ingredient> {

    @Query("select new com.vz.backend.core.dto.OrgGroupDto(o.name, o.id) FROM Calendar2Ingredient og " +
            " inner join Organization o on og.objectId=o.id and og.type=:type and o.clientId=:clientId and og.calendarId=:calendarId")
    List<OrgGroupDto> getListOrgIngredient(ReceiveTypeEnum type, long clientId, long calendarId);

    @Query("select new com.vz.backend.core.dto.OrgGroupDto(o.name, o.id) FROM Calendar2Ingredient og " +
            " inner join Group o on og.objectId=o.id and og.type=:type and o.clientId=:clientId and og.calendarId=:calendarId")
    List<OrgGroupDto> getListGroupIngredient(ReceiveTypeEnum type, long clientId, long calendarId);

    @Query("select new com.vz.backend.core.dto.UserBasicDto(u) FROM Calendar2Ingredient og " +
            " inner join User u on og.objectId=u.id and og.type=:type and u.clientId=:clientId and og.calendarId=:calendarId")
    List<UserBasicDto> getListUserIngredient(ReceiveTypeEnum type, long clientId, long calendarId);

    @Query("SELECT NEW com.vz.backend.core.dto.OrgGroupDto(o.name, o.id) FROM Organization o WHERE o.active IS TRUE AND o.clientId = :clientId AND o.id IN :orgIds")
	List<OrgGroupDto> findOrgByListId(Long clientId, List<Long> orgIds);
    
    @Query("SELECT NEW com.vz.backend.core.dto.OrgGroupDto(g.name, g.id) FROM Group g WHERE g.active IS TRUE AND g.clientId = :clientId AND g.id IN :groupIds")
	List<OrgGroupDto> findGroupByListId(Long clientId, List<Long> groupIds);

    @Query("SELECT o FROM Calendar2Ingredient o WHERE o.clientId = :clientId AND o.calendarId =:calendarId")
    List<Calendar2Ingredient> findOrgByCalendarId(Long clientId, Long calendarId);

}
