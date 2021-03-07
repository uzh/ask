let questions =
  Sihl.Migration.create_step
    ~label:"questions"
    {sql|
      CREATE TABLE IF NOT EXISTS `quest_questions` (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `label` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
        `help_text` varchar(512) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
        `text` varchar(512) COLLATE utf8mb4_unicode_ci NOT NULL,
        `default_value` varchar(512) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
        `validation_regex` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT '.+',
        `question_type` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT 'text',
        `max_file_size_mb` int(11) DEFAULT NULL,
        `mime_types` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
        `possible_options` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
        `created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (`id`),
      UNIQUE KEY `unique_uuid` (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
    |sql}
;;

let templates =
  Sihl.Migration.create_step
    ~label:"templates"
    {sql|
      CREATE TABLE IF NOT EXISTS `quest_templates` (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `label` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL,
        `description` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
        `created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (`id`),
      UNIQUE KEY `unique_uuid` (`uuid`),
      UNIQUE KEY `unique_label` (`label`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
    |sql}
;;

let questionnaires =
  Sihl.Migration.create_step
    ~label:"questionnaires"
    {sql|
      CREATE TABLE IF NOT EXISTS `quest_questionnaires` (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `quest_template` bigint(20) unsigned NOT NULL,
        `created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (`id`),
      UNIQUE KEY `unique_uuid` (`uuid`),
      KEY `quest_template` (`quest_template`),
      CONSTRAINT `quest_questionnaires_ibfk_1` FOREIGN KEY (`quest_template`) REFERENCES `quest_templates` (`id`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
    |sql}
;;

let mappings =
  Sihl.Migration.create_step
    ~label:"mappings"
    {sql|
      CREATE TABLE IF NOT EXISTS `quest_template_question_mappings` (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `quest_template` bigint(20) unsigned NOT NULL,
        `quest_question` bigint(20) unsigned NOT NULL,
        `question_order` bigint(20) unsigned NOT NULL,
        `required` tinyint(1) DEFAULT '0',
        `created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (`id`),
      UNIQUE KEY `unique_uuid` (`uuid`),
      UNIQUE KEY `unique_order` (`quest_template`, `question_order`),
      KEY `quest_question` (`quest_question`),
      CONSTRAINT `quest_template_question_mappings_ibfk_1` FOREIGN KEY (`quest_template`) REFERENCES `quest_templates` (`id`),
      CONSTRAINT `quest_template_question_mappings_ibfk_2` FOREIGN KEY (`quest_question`) REFERENCES `quest_questions` (`id`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
    |sql}
;;

let answers =
  Sihl.Migration.create_step
    ~label:"answers"
    ~check_fk:false
    {sql|
    CREATE TABLE IF NOT EXISTS `quest_answers` (
      `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
      `uuid` binary(16) NOT NULL,
      `quest_questionnaire` bigint(20) unsigned NOT NULL,
      `quest_template_question_mapping` bigint(20) unsigned NOT NULL,
      `text` varchar(5000) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
      `storage_handle` binary(16) NULL,
      `created` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
      `updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (`id`),
      UNIQUE KEY `unique_uuid` (`uuid`),
      KEY `quest_questionnaire` (`quest_questionnaire`),
      KEY `quest_template_question_mapping` (`quest_template_question_mapping`),
      KEY `storage_handle` (`storage_handle`),
      CONSTRAINT `quest_answers_ibfk_1` FOREIGN KEY (`quest_questionnaire`) REFERENCES `quest_questionnaires` (`id`),
      CONSTRAINT `quest_answers_ibfk_2` FOREIGN KEY (`quest_template_question_mapping`) REFERENCES `quest_template_question_mappings` (`id`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
    |sql}
;;

let migration () =
  let open Sihl.Migration in
  empty "attributes"
  |> add_step questions
  |> add_step templates
  |> add_step questionnaires
  |> add_step mappings
  |> add_step answers
;;
