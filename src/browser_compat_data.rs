use std::collections::HashMap;

use serde::Deserialize;
use serde_with::{OneOrMany, serde_as};

const DATA: &str =
    include_str!("../browser-compat-data/node_modules/@mdn/browser-compat-data/data.json");

#[derive(Debug, Deserialize)]
struct BrowserCompatData {
    javascript: JavascriptData,
}

#[derive(Debug, Deserialize)]
struct JavascriptData {
    __compat: Option<CompatData>,
    #[serde(flatten)]
    path: HashMap<String, JavascriptData>,
}

#[derive(Debug, Deserialize)]
pub struct CompatData {
    pub support: Support,
}

#[serde_as]
#[derive(Debug, Deserialize)]
pub struct Support {
    #[serde_as(as = "OneOrMany<_>")]
    pub chrome: Vec<BrowserSupport>,
}

#[derive(Debug, Deserialize)]
pub struct BrowserSupport {
    pub version_added: VersionAdded,
    #[serde(default)]
    pub partial_implementation: bool,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum VersionAdded {
    Version(String),
    NotAdded(NotAdded),
}

#[derive(Debug, Deserialize)]
#[serde(try_from = "bool")]
pub struct NotAdded;

impl TryFrom<bool> for NotAdded {
    type Error = &'static str;

    fn try_from(value: bool) -> Result<Self, Self::Error> {
        match value {
            false => Ok(Self),
            true => Err("`true` value used for `version_added`"),
        }
    }
}

pub struct Data {
    data: BrowserCompatData,
}

impl Data {
    pub fn new() -> Self {
        let data: BrowserCompatData = serde_json::from_str(DATA).unwrap();

        fn aux(data: &JavascriptData) {
            if let Some(compat) = &data.__compat {
                let n = compat.support.chrome.len();
                assert!(1 <= n);
                assert!(n <= 2);
            }
            for v in data.path.values() {
                aux(v);
            }
        }

        aux(&data.javascript);

        Self { data }
    }

    pub fn compat_data<'a>(&'a self, js_feature: &[&'static str]) -> &'a CompatData {
        fn aux<'a>(data: &'a JavascriptData, path: &[&'static str]) -> &'a CompatData {
            match path.split_first() {
                None => data.__compat.as_ref().unwrap(),
                Some((first, rest)) => aux(data.path.get(*first).unwrap(), rest),
            }
        }

        aux(&self.data.javascript, js_feature)
    }
}
