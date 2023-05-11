package com.vz.backend.business.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.business.domain.ObjectTag;
import com.vz.backend.business.domain.Tag;
import com.vz.backend.business.dto.ObjectTagDto;
import com.vz.backend.business.service.ObjectTagService;
import com.vz.backend.business.service.TagService;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DocumentTypeEnum;

@RestController
@RequestMapping("/tag")
public class TagController {

	@Autowired
	private TagService tagService;

	@Autowired
	private ObjectTagService objTagService;

	@PostMapping("/add")
	public ResponseEntity<Tag> add(@RequestBody Tag tag) {
		return new ResponseEntity<>(tagService.addTag(tag), HttpStatus.OK);
	}

	@PostMapping("/update/{id}")
	public ResponseEntity<Tag> update(@PathVariable Long id, @RequestParam String name) {
		return new ResponseEntity<>(tagService.update(id, name), HttpStatus.OK);
	}

	@PostMapping("/delete/{id}")
	public ResponseEntity<Boolean> update(@PathVariable Long id) {
		return new ResponseEntity<>(tagService.delete(id), HttpStatus.OK);
	}

	@PostMapping("/assign")
	public ResponseEntity<ObjectTag> assign(@RequestParam Long tagId, @RequestParam Long objId,
			@RequestParam DocumentTypeEnum type) {
		return new ResponseEntity<>(objTagService.assign(tagId, objId, type), HttpStatus.OK);
	}

	@PostMapping("/remove")
	public ResponseEntity<Boolean> remove(@RequestParam Long objId, @RequestParam Long tagId, @RequestParam DocumentTypeEnum type) {
		return new ResponseEntity<>(objTagService.untag(objId, tagId, type), HttpStatus.OK);
	}

	@PostMapping("/list")
	public ResponseEntity<Page<Tag>> getListTag(@RequestParam boolean detailPageSize,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		return new ResponseEntity<>(tagService.getListTagByCreator(detailPageSize, page), HttpStatus.OK);
	}

	@GetMapping("/object/{tagId}")
	public ResponseEntity<Page<ObjectTagDto>> getListObjectTag(@PathVariable Long tagId,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page,
			@RequestParam("keyWord") String keyWord) {
		return new ResponseEntity<>(objTagService.getListObjectTagByTagId(tagId, page,keyWord), HttpStatus.OK);
	}

	@GetMapping("/search")
	public ResponseEntity<Page<Tag>> searchTag(@RequestParam(required = false) String text, @RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		return new ResponseEntity<>(tagService.search(text, page), HttpStatus.OK);
	}
	
	@GetMapping("/search/unpage")
	public ResponseEntity<List<Tag>> searchTagWithouPage(@RequestParam(required = false) String text) {
		return new ResponseEntity<>(tagService.searchWithoutPage(text), HttpStatus.OK);
	}
	
	@GetMapping("/object_tag")
	public ResponseEntity<List<Tag>> getListTagByObjId(@RequestParam Long objId, @RequestParam DocumentTypeEnum type) {
		return new ResponseEntity<>(tagService.getListTagByObjId(objId, type), HttpStatus.OK);
	}
	
	@PostMapping("/list/unpage")
	public ResponseEntity<List<Tag>> getListTagWithoutPage() {
		return new ResponseEntity<>(tagService.getListTagByCreatorWithoutPage(), HttpStatus.OK);
	}
}
